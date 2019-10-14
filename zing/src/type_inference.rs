
use std::collections::{HashMap, HashSet};
use std::iter::repeat;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::{CompileError, Compiler, Location};
use crate::names::*;

pub fn infer_types<'ast, 'input>(
		program: &mut Program<'ast>,
		names: &'ast Names<'ast>,
		compiler: &mut Compiler<'input>) -> Result<(), CompileError> {
	let mut type_inferrer = TypeInferrer::new(names, compiler);
	type_inferrer.infer_signatures(program)?;
	type_inferrer.infer_bodies(program)?;
	Ok(())
}


struct TypeInferrer<'ast, 'input, 'comp> {
	names: &'ast Names<'ast>,
	compiler: &'comp mut Compiler<'input>,
	signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,

	// Procedure-local state
	ready: HashMap<&'ast str, TypeResult>,
	current_decl_index: usize,
	current_is_main: bool,
}

#[derive(Clone, Copy, Debug)]
enum TypeResult {
	Type { inferred_type: Type },
	Error,
}

impl<'ast, 'input, 'comp> TypeInferrer<'ast, 'input, 'comp> {
	pub fn new(names: &'ast Names<'ast>, compiler: &'comp mut Compiler<'input>)
			-> TypeInferrer<'ast, 'input, 'comp> {
		TypeInferrer {
			names,
			compiler,
			signatures: vec![],
			ready: HashMap::new(),
			current_decl_index: 0,
			current_is_main: false,
		}
	}

	pub fn infer_signatures(&mut self, program: &mut Program<'ast>) -> Result<(), CompileError> {
		for decl in &mut program.declarations {
			let Declaration::Procedure { kind, name, inputs, outputs, .. } = decl;
			let mut instrument_types: Vec<Type> = vec![];
			let mut output_count = 0;
			let mut seen_generic_input = false;
			let input_iter = repeat(false).zip(&mut inputs.items);
			let output_iter = repeat(true).zip(&mut outputs.items);
			for (is_output, item) in input_iter.chain(output_iter) {
				let PatternItem { variable, item_type } = item;
				match kind {
					// Module inputs/outputs default to dynamic stereo number, and only
					// inputs can be static.
					ProcedureKind::Module => {
						item_type.scope = item_type.scope.or(Some(Scope::Dynamic));
						if is_output {
							if let Some(Scope::Static) = item_type.scope {
								self.compiler.report_error(&*variable,
									"Module outputs can't be static.");
							}
						}
						item_type.width = item_type.width.or(Some(Width::Stereo));
						item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
					},
					// Function inputs/outputs default to stereo number and have no
					// static/dynamic distinction.
					ProcedureKind::Function => {
						if item_type.scope.is_some() {
							self.compiler.report_error(&*variable,
								"Function inputs or outputs can't be marked static or dynamic.");
						}
						item_type.width = item_type.width.or(Some(Width::Stereo));
						item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
					},
					// Instrument inputs default to dynamic stereo number, only
					// inputs can be static, all static inputs must appear before all
					// dynamic inputs, there must be the same number of dynamic inputs
					// and outputs, the types of these must match up, and outputs
					// default to the types of their corresponding inputs.
					ProcedureKind::Instrument => {
						item_type.scope = item_type.scope.or(Some(Scope::Dynamic));
						if is_output {
							match item_type.scope.unwrap() {
								Scope::Static => {
									self.compiler.report_error(&*variable,
										"Instrument outputs can't be static.");
								},
								Scope::Dynamic => {
									if output_count < instrument_types.len() {
										let input = &instrument_types[output_count];
										item_type.width = item_type.width.or(input.width);
										item_type.value_type = item_type.value_type.or(input.value_type);
										if item_type != input {
											self.compiler.report_error(&*variable,
												"Instrument output type doesn't match the corresponding input.");
										}
									} else if output_count == instrument_types.len() {
										self.compiler.report_error(&*variable,
											"Instrument has more outputs than dynamic inputs.");
									}
									output_count += 1;
								},
							}
						} else {
							item_type.width = item_type.width.or(Some(Width::Stereo));
							item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
							match item_type.scope.unwrap() {
								Scope::Static => {
									if !instrument_types.is_empty() {
										self.compiler.report_error(&*variable,
											"Static instrument inputs can't come after dynamic inputs.");
									}
								},
								Scope::Dynamic => {
									instrument_types.push(*item_type);
								},
							}
						}
					},
				}
				if item_type.width == Some(Width::Generic) {
					if is_output {
						if !seen_generic_input {
							self.compiler.report_error(&*variable,
								"Outputs can't be generic when none of the inputs are generic.");
						}
					} else {
						seen_generic_input = true;
					}
				}
			}
			if output_count < instrument_types.len() {
				self.compiler.report_error(&*name, "Instrument has fewer outputs than dynamic inputs.");
			}

			// Extract signature
			let Declaration::Procedure { kind, inputs, outputs, .. } = decl;
			let input_types = inputs.items.iter().map(|item| {
				let mut input_type = item.item_type;
				if *kind == ProcedureKind::Instrument {
					// Seen from the outside, all instrument inputs are dynamic.
					input_type.scope = Some(Scope::Dynamic);
				}
				input_type
			}).collect();
			let output_types = outputs.items.iter().map(|item| item.item_type).collect();
			self.signatures.push((*kind, input_types, output_types));
		}
		self.compiler.check_errors()
	}

	pub fn infer_bodies(&mut self, program: &mut Program<'ast>) -> Result<(), CompileError> {
		for (decl_index, decl) in program.declarations.iter_mut().enumerate() {
			self.current_decl_index = decl_index;
			self.infer_body(decl)?;
			self.check_outputs(decl)?;
		}
		Ok(())
	}

	fn check_outputs(&mut self, decl: &mut Declaration<'ast>) -> Result<(), CompileError> {
		let Declaration::Procedure { outputs, .. } = decl;
		for output in &outputs.items {
			let mut check_output = |name: &Id<'ast>, output_type: &Type| {
				match self.ready.get(name.text) {
					Some(&TypeResult::Type { inferred_type }) => {
						if !inferred_type.assignable_to(output_type) {
							self.compiler.report_error(name,
								format!("Expression of type{} can't be assigned to output '{}'{}.",
								inferred_type, name, output_type));
						}
					},
					Some(&TypeResult::Error) => {},
					None => {
						self.compiler.report_error(name,
							format!("No assignment to output '{}'.", name));
					},
				}
			};
			match &output.variable {
				PatternVariable::Variable { name } => {
					check_output(name, &output.item_type);
				},
				PatternVariable::Split { left, right } => {
					let mut side_type = output.item_type;
					side_type.width = Some(Width::Mono);
					check_output(left, &side_type);
					check_output(right, &side_type);
				},
			}
		}

		self.compiler.check_errors()
	}

	fn infer_body(&mut self, decl: &mut Declaration<'ast>) -> Result<(), CompileError> {
		let Declaration::Procedure { kind, name, inputs, body, .. } = decl;
		self.current_is_main = name.text == "main";
		if self.current_is_main && *kind != ProcedureKind::Module {
			self.compiler.report_error(&*name, "'main' must be a module.");
		}

		// Record dependencies of all statements and mark inputs and fully typed
		// variables as ready.
		// A statement can be inferred when all its dependencies are ready.
		self.ready.clear();
		let mut dependencies = vec![HashSet::new(); body.len()];
		self.find_ready(*kind, inputs);
		for (body_index, Statement::Assign { node, exp }) in body.iter_mut().enumerate() {
			self.find_dependencies(exp, &mut dependencies[body_index]);
			self.find_ready(*kind, node);
		}
		self.compiler.check_errors()?;

		let mut done = vec![false; body.len()];
		let mut done_count = 0;
		while done_count < body.len() {
			let mut changed = false;
			for (body_index, Statement::Assign { node, exp }) in body.iter_mut().enumerate() {
				if !done[body_index] && dependencies[body_index].iter()
						.all(|name| self.ready.contains_key(name)) {
					self.infer_statement(node, exp);
					done[body_index] = true;
					done_count += 1;
					changed = true;
				}
			}
			if !changed {
				// There are cycles. Require width and infer rest of type.
				for (body_index, Statement::Assign { node, .. }) in body.iter_mut().enumerate() {
					if !done[body_index] {
						for PatternItem { variable, item_type } in &mut node.items {
							let Type { scope, width, value_type } = item_type;
							if *kind != ProcedureKind::Function {
								if scope.is_none() {
									*scope = Some(Scope::Dynamic);
								}
							}
							if width.is_none() {
								self.compiler.report_error(&*variable,
									"Variables depending on a cycle must specify explict width (mono, stereo or generic).");
								*width = Some(Width::Stereo);
							}
							if value_type.is_none() {
								*value_type = Some(ValueType::Number);
							}
						}
						self.find_ready(*kind, node);
					}
				}
			}
		}

		self.compiler.check_errors()
	}

	fn find_ready(&mut self,
			proc_kind: ProcedureKind,
			pattern: &mut Pattern<'ast>) {
		for PatternItem { variable, item_type } in &mut pattern.items {
			let Type { scope, width, value_type } = item_type;
			// A split pattern can only be used for stereo values, and buffer
			// values can't be split.
			if let PatternVariable::Split { .. } = variable {
				if width.is_none() {
					*width = Some(Width::Stereo);
				} else if *width != Some(Width::Stereo) {
					self.compiler.report_error(&*variable, "Only stereo values can be split.");
				}
				if *value_type == Some(ValueType::Buffer) {
					self.compiler.report_error(&*variable, "Buffer values can't be split.");
				}
			}
			// Mark variable ready if it is fully typed.
			let is_ready = match proc_kind {
				ProcedureKind::Function => {
					if scope.is_some() {
						self.compiler.report_error(&*variable,
							"Variables in functions can't be marked static or dynamic.");
					}
					width.is_some() && value_type.is_some()
				},
				_ => scope.is_some() && width.is_some() && value_type.is_some(),
			};
			if is_ready {
				self.make_ready(variable, TypeResult::Type {
					inferred_type: *item_type,
				});
			}
		}
	}

	fn make_ready(&mut self,
			variable: &PatternVariable<'ast>,
			mut result: TypeResult) {
		match variable {
			PatternVariable::Variable { name } => {
				self.ready.insert(name.text, result);
			},
			PatternVariable::Split { left, right } => {
				if let TypeResult::Type { inferred_type } = &mut result {
					inferred_type.width = Some(Width::Mono);
				}
				self.ready.insert(left.text, result);
				self.ready.insert(right.text, result);
			}
		}
	}

	fn find_dependencies(&mut self,
			exp: &Expression<'ast>,
			dependencies: &mut HashSet<&'ast str>) {
		exp.traverse_pre(&mut |exp| {
			if let Expression::Variable { name } = exp {
				match self.names.lookup_variable(self.current_decl_index, name.text) {
					Some(..) => {
						dependencies.insert(name.text);
					},
					None => {
						self.compiler.report_error(name,
							format!("Variable not found: '{}'.", name));
					},
				}
			}
		});
	}

	fn infer_statement(&mut self,
			node: &mut Pattern<'ast>,
			exp: &mut Expression<'ast>) {
		let exp_types = self.infer_expression(exp);
		if node.items.len() != exp_types.len() {
			self.compiler.report_error(node,
				format!("Mismatching number of values: {} in pattern, {} in expression.",
					node.items.len(), exp_types.len()));
		}
		let node_iter = node.items.iter_mut();
		let exp_iter = exp_types.iter().chain(repeat(&TypeResult::Error));
		for (item, exp_type) in node_iter.zip(exp_iter) {
			let mut node_type = item.item_type;
			let result = match exp_type {
				TypeResult::Type { inferred_type } => {
					node_type.scope = node_type.scope.or(inferred_type.scope);
					node_type.width = node_type.width.or(inferred_type.width);
					node_type.value_type = node_type.value_type.or(inferred_type.value_type);
					if inferred_type.assignable_to(&node_type) {
						item.item_type = node_type;
						TypeResult::Type { inferred_type: node_type }
					} else {
						self.compiler.report_error(&item.variable,
							format!("Expression of type{} can't be assigned to '{}'{}.",
								inferred_type, item.variable, item.item_type));
						TypeResult::Error
					}
				},
				TypeResult::Error => {
					TypeResult::Error
				},
			};
			self.make_ready(&item.variable, result);
		}
	}

	fn infer_expression(&mut self,
			exp: &Expression<'ast>) -> Vec<TypeResult> {
		let loc = exp as &dyn Location;
		use Expression::*;
		match exp {
			Number { value, .. } => self.infer_number(value, loc),
			Bool { value, .. } => self.infer_bool(value, loc),
			Variable { name } => self.infer_variable(name, loc),
			UnOp { op, exp } => self.infer_unop(op, exp, loc),
			BinOp { left, op, right } => self.infer_binop(left, op, right, loc),
			Conditional { condition, then, otherwise }
				=> self.infer_conditional(condition, then, otherwise, loc),
			Call { name, args, .. } => self.infer_call(name, args, loc),
			Tuple { elements, .. } => self.infer_tuple(elements, loc),
			Merge { left, right, .. } => self.infer_merge(left, right, loc),
			Property { exp, name } => self.infer_property(exp, name, loc),
			TupleIndex { exp, index, .. } => self.infer_tuple_index(exp, *index, loc),
			BufferIndex { exp, index, .. } => self.infer_buffer_index(exp, index, loc),
		}
	}

	fn expect_single(&mut self,
			exp: &Expression<'ast>,
			title: &str) -> TypeResult {
		let result = self.infer_expression(exp);
		if result.len() != 1 {
			self.compiler.report_error(exp, format!("Single value expected for {}.", title));
			TypeResult::Error
		} else {
			result[0]
		}
	}

	fn infer_number(&mut self,
			_value: &f64,
			_loc: &dyn Location) -> Vec<TypeResult> {
		let inferred_type = Type {
			scope: None,
			width: Some(Width::Mono),
			value_type: Some(ValueType::Number),
		};
		vec![TypeResult::Type { inferred_type }]
	}

	fn infer_bool(&mut self,
			_value: &bool,
			_loc: &dyn Location) -> Vec<TypeResult> {
		let inferred_type = Type {
			scope: None,
			width: Some(Width::Mono),
			value_type: Some(ValueType::Bool),
		};
		vec![TypeResult::Type { inferred_type }]
	}

	fn infer_variable(&mut self,
			name: &Id<'ast>,
			_loc: &dyn Location) -> Vec<TypeResult> {
		vec![self.ready[name.text]]
	}

	fn infer_unop(&mut self,
			op: &UnOp, exp: &Expression<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let operand = self.expect_single(exp, "operand of unary operator");
		self.check_signature(op.signature(), &[operand], loc)
	}

	fn infer_binop(&mut self,
			left: &Expression<'ast>, op: &BinOp, right: &Expression<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let left_operand = self.expect_single(left, "left operand of binary operator");
		let right_operand = self.expect_single(right, "right operand of binary operator");
		self.check_signature(op.signature(), &[left_operand, right_operand], loc)
	}

	fn infer_conditional(&mut self,
			condition: &Expression<'ast>, then: &Expression<'ast>, otherwise: &Expression<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let condition_operand = self.expect_single(condition, "condition of conditional operator");
		let then_operand = self.expect_single(then, "operand of conditional operator");
		let otherwise_operand = self.expect_single(otherwise, "operand of conditional operator");
		let sig = sig!([generic bool, generic typeless, generic typeless] [generic typeless]);
		let result = self.check_signature(&sig, &[condition_operand, then_operand, otherwise_operand], loc);
		if let TypeResult::Type {
			inferred_type: Type { value_type: Some(ValueType::Buffer), .. }
		} = result[0] {
			if let TypeResult::Type {
				inferred_type: Type { width, .. }
			} = condition_operand {
				if width != Some(Width::Mono) {
					self.compiler.report_error(loc,
						"Conditionals on buffers must have mono conditions.");
					return vec![TypeResult::Error];
				}
			}
		}
		result
	}

	fn infer_call(&mut self,
			name: &Id<'ast>, args: &Vec<Expression<'ast>>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let mut operands = vec![];
		for arg in args {
			operands.append(&mut self.infer_expression(&arg));
		}
		match self.names.lookup_procedure(name.text) {
			Some(ProcedureRef { kind, definition }) => {
				match kind {
					ProcedureKind::Module => {
						let (current_kind, _, _) = self.signatures[self.current_decl_index];
						if current_kind == ProcedureKind::Function {
							self.compiler.report_error(name,
								"Modules can't be called from functions.");
						}
					},
					ProcedureKind::Function => {},
					ProcedureKind::Instrument => {
						if !self.current_is_main {
							self.compiler.report_error(name,
								"Instruments can only be called from the main module.");
						}
					},
				}
				match definition {
					ProcedureDefinition::BuiltIn { signature } => {
						self.check_call_signature(signature, &operands, loc)
					}
					ProcedureDefinition::Declaration { decl_index } => {
						let (_, ref inputs, ref outputs) = self.signatures[*decl_index].clone();
						let sig = &Signature { inputs, outputs };
						self.check_call_signature(sig, &operands, loc)
					},
				}
			},
			None => {
				self.compiler.report_error(name, format!("Procedure not found: '{}'.", name));
				vec![TypeResult::Error]
			},
		}
	}

	fn infer_tuple(&mut self,
			elements: &Vec<Expression<'ast>>,
			_loc: &dyn Location) -> Vec<TypeResult> {
		let mut result = vec![];
		for exp in elements {
			result.append(&mut self.infer_expression(exp));
		}
		result
	}

	fn infer_merge(&mut self,
			left: &Expression<'ast>, right: &Expression<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let left_operand = self.expect_single(left, "left operand of merge");
		let right_operand = self.expect_single(right, "right operand of merge");
		let merge_sig = sig!([mono typeless, mono typeless] [stereo typeless]);
		self.check_signature(&merge_sig, &[left_operand, right_operand], loc)
	}

	fn infer_property(&mut self,
			exp: &Expression<'ast>, name: &Id<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		if name.text == "left" {
			let operand = self.expect_single(exp, "operand of 'left' property");
			let left_sig = sig!([stereo typeless] [mono typeless]);
			self.check_signature(&left_sig, &[operand], loc)
		} else if name.text == "right" {
			let operand = self.expect_single(exp, "operand of 'right' property");
			let right_sig = sig!([stereo typeless] [mono typeless]);
			self.check_signature(&right_sig, &[operand], loc)
		} else if name.text == "length" {
			let operand = self.expect_single(exp, "operand of 'length' property");
			let length_sig = sig!([generic buffer] [mono number]);
			self.check_signature(&length_sig, &[operand], loc)
		} else {
			self.compiler.report_error(name, format!("Invalid property '{}'.", name));
			vec![TypeResult::Error]
		}
	}

	fn infer_tuple_index(&mut self,
			exp: &Expression<'ast>, index: u64,
			loc: &dyn Location) -> Vec<TypeResult> {
		let tuple = self.infer_expression(exp);
		let index = index as usize;
		if index < tuple.len() {
			vec![tuple[index]]
		} else {
			self.compiler.report_error(loc,
				format!("Index {} outside range of tuple of length {}.", index, tuple.len()));
			vec![TypeResult::Error]
		}
	}

	fn infer_buffer_index(&mut self,
			exp: &Expression<'ast>, index: &Expression<'ast>,
			loc: &dyn Location) -> Vec<TypeResult> {
		let buffer_operand = self.expect_single(exp, "buffer operand of indexing");
		let index_operand = self.expect_single(index, "index operand of indexing");
		let index_sig = sig!([generic buffer, mono number] [generic number]);
		self.check_signature(&index_sig, &[buffer_operand, index_operand], loc)
	}

	fn check_call_signature(&mut self, sig: &Signature, args: &[TypeResult],
			loc: &dyn Location) -> Vec<TypeResult> {
		if sig.inputs.len() == args.len() {
			self.check_signature(sig, args, loc)
		} else {
			self.compiler.report_error(loc,
				format!("{} arguments expected, {} given.", sig.inputs.len(), args.len()));
			vec![TypeResult::Error; sig.outputs.len()]
		}
	}

	fn check_signature(&mut self, sig: &Signature, args: &[TypeResult],
			loc: &dyn Location) -> Vec<TypeResult> {
		let mut seen_static = false;
		let mut seen_dynamic = false;
		let mut seen_stereo = false;
		let mut seen_generic = false;
		let mut seen_number = false;
		let mut seen_bool = false;
		let mut seen_buffer = false;
		let mut seen_error = false;
		for (sig_type, arg_type_result) in sig.inputs.iter().zip(args) {
			match arg_type_result {
				TypeResult::Type { inferred_type: arg_type } => {
					match (sig_type.scope, arg_type.scope) {
						(None, Some(Scope::Static)) => seen_static = true,
						(None, Some(Scope::Dynamic)) => seen_dynamic = true,
						(Some(Scope::Static), Some(Scope::Dynamic)) => {
							self.compiler.report_error(loc,
								"Can't pass a dynamic value into a static input.");
						},
						_ => {},
					}
					match (sig_type.width.unwrap(), arg_type.width.unwrap()) {
						(Width::Generic, Width::Stereo) => seen_stereo = true,
						(Width::Generic, Width::Generic) => seen_generic = true,
						(_, Width::Mono) => {},
						(s, a) => if s != a {
							self.compiler.report_error(loc,
								format!("Can't pass a {} value into a {} input.", a, s));
						},
					}
					if sig_type.value_type.unwrap() == ValueType::Typeless {
						match arg_type.value_type.unwrap() {
							ValueType::Number => seen_number = true,
							ValueType::Bool => seen_bool = true,
							ValueType::Buffer => seen_buffer = true,
							ValueType::Typeless => panic!(),
						}
					} else if sig_type.value_type != arg_type.value_type {
						self.compiler.report_error(loc,
							format!("Can't pass a {} value into a {} input.",
								arg_type.value_type.unwrap(), sig_type.value_type.unwrap()));
					}
				},
				TypeResult::Error => {
					seen_error = true;
				},
			}
		}
		if seen_error {
			return vec![TypeResult::Error; sig.outputs.len()];
		}
		sig.outputs.iter().map(|out_type| {
			let scope = match (out_type.scope, seen_static, seen_dynamic) {
				(None, _, true) => Some(Scope::Dynamic),
				(None, true, false) => Some(Scope::Static),
				(None, false, false) => None,
				(Some(s), _, _) => Some(s),
			};
			let width = match (out_type.width.unwrap(), seen_stereo, seen_generic) {
				(Width::Generic, false, false) => Some(Width::Mono),
				(Width::Generic, true, false) => Some(Width::Stereo),
				(Width::Generic, false, true) => Some(Width::Generic),
				(Width::Generic, true, true) => {
					self.compiler.report_error(loc,
						"Can't mix stereo and generic values for generic inputs.");
					None
				},
				(w, _, _) => Some(w),
			};
			let value_type = match (out_type.value_type.unwrap(), seen_number, seen_bool, seen_buffer) {
				(ValueType::Typeless, true, false, false) => Some(ValueType::Number),
				(ValueType::Typeless, false, true, false) => Some(ValueType::Bool),
				(ValueType::Typeless, false, false, true) => Some(ValueType::Buffer),
				(ValueType::Typeless, _, _, _) => {
					self.compiler.report_error(loc,
						"Can't mix different types for typeless inputs.");
					None
				},
				(t, _, _, _) => Some(t),
			};
			if width.is_some() && value_type.is_some() {
				TypeResult::Type { inferred_type: Type {
					scope, width, value_type
				}}
			} else {
				TypeResult::Error
			}
		}).collect()
	}
}
