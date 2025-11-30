
use std::collections::{HashMap, HashSet};
use std::iter::repeat;
use std::mem::replace;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::{CompileError, Compiler, Location, PosRange};
use crate::names::*;

#[derive(Clone, Debug)]
pub struct FullSignature {
	pub context: Context,
	pub kind: ProcedureKind,
	pub midi_input_count: usize,
	pub inputs: Vec<Type>,
	pub outputs: Vec<Type>,
}

pub fn infer_types<'comp, 'names>(
		program: &mut Program,
		names: &'names Names,
		compiler: &'comp mut Compiler)
		-> Result<(
			Vec<FullSignature>,
			HashMap<*const Expression, Width>,
			Vec<Vec<usize>>,
			Vec<Vec<*const PrecompiledProcedure>>,
		), CompileError> {
	let mut type_inferrer = TypeInferrer::new(names, compiler);
	type_inferrer.infer_signatures(program)?;
	type_inferrer.infer_bodies(program)?;
	Ok((type_inferrer.signatures, type_inferrer.stored_widths, type_inferrer.callees, type_inferrer.precompiled_callees))
}


struct TypeInferrer<'comp, 'names> {
	names: &'names Names,
	compiler: &'comp mut Compiler,
	signatures: Vec<FullSignature>,
	stored_widths: HashMap<*const Expression, Width>,
	parameters: HashMap<String, TypeResult>,
	callees: Vec<Vec<usize>>,
	precompiled_callees: Vec<Vec<*const PrecompiledProcedure>>,

	// Procedure-local state
	ready: HashMap<String, TypeResult>,
	current_proc_index: usize,
	current_proc_name_loc: PosRange,
}

#[derive(Clone, Copy, Debug)]
enum TypeResult {
	Type { inferred_type: Type },
	Error,
}

impl TypeResult {
	fn width(&self) -> Option<Width> {
		match self {
			TypeResult::Type { inferred_type } => inferred_type.width,
			TypeResult::Error => None,
		}
	}
}

impl<'comp, 'names> TypeInferrer<'comp, 'names> {
	pub fn new(names: &'names Names, compiler: &'comp mut Compiler)
			-> TypeInferrer<'comp, 'names> {
		let mut parameters = HashMap::new();
		for param in names.parameter_names() {
			parameters.insert(param.clone(), TypeResult::Type {
				inferred_type: type_spec!(dynamic mono number),
			});
		}
		TypeInferrer {
			names,
			compiler,
			signatures: vec![],
			stored_widths: HashMap::new(),
			parameters,
			callees: vec![],
			precompiled_callees: vec![],
			ready: HashMap::new(),
			current_proc_index: 0,
			current_proc_name_loc: PosRange::default(),
		}
	}

	pub fn infer_signatures(&mut self, program: &mut Program) -> Result<(), CompileError> {
		for proc in &mut program.procedures {
			let mut instrument_types: Vec<Type> = vec![];
			let mut output_count = 0;
			let mut seen_generic_input = false;
			let input_iter = repeat(false).zip(&mut proc.inputs.items);
			let output_iter = repeat(true).zip(&mut proc.outputs.items);
			for (is_output, item) in input_iter.chain(output_iter) {
				let PatternItem { name: variable_name, item_type } = item;
				match proc.kind {
					// Module inputs/outputs default to dynamic stereo number, and only
					// inputs can be static.
					ProcedureKind::Module => {
						if is_output {
							if let Some(Scope::Static) = item_type.scope {
								self.compiler.report_error(&*variable_name,
									"Module outputs can't be static.");
							}
						}
						item_type.inherit(&type_spec!(dynamic stereo number));
					},
					// Function inputs/outputs default to stereo number and have no
					// static/dynamic distinction.
					ProcedureKind::Function => {
						if item_type.scope.is_some() {
							self.compiler.report_error(&*variable_name,
								"Function inputs or outputs can't be marked static or dynamic.");
						}
						item_type.inherit(&type_spec!(stereo number));
					},
					// Instrument inputs default to dynamic stereo number, only
					// inputs can be static, all static inputs must appear before all
					// dynamic inputs, there must be exactly one output, and the type
					// of that output must be stereo number.
					ProcedureKind::Instrument => {
						if is_output {
							match item_type.scope {
								Some(Scope::Static) => {
									self.compiler.report_error(&*variable_name,
										"Instrument outputs can't be static.");
								},
								Some(Scope::Dynamic) | None => {
									item_type.inherit(&type_spec!(dynamic stereo number));
									if *item_type != type_spec!(dynamic stereo number) {
										self.compiler.report_error(&*variable_name,
											"Instrument output type must be stereo number.");
									}
									output_count += 1;
								},
							}
						} else {
							item_type.inherit(&type_spec!(dynamic stereo number));
							match item_type.scope.unwrap() {
								Scope::Static => {
									if !instrument_types.is_empty() {
										self.compiler.report_error(&*variable_name,
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
							self.compiler.report_error(&*variable_name,
								"Outputs can't be generic when none of the inputs are generic.");
						}
					} else {
						seen_generic_input = true;
					}
				}
			}
			if proc.kind == ProcedureKind::Instrument && output_count != 1 {
				self.compiler.report_error(&proc.name, "Instruments must have exactly one output.");
			}

			// Extract signature
			let input_types = proc.inputs.items.iter().map(|item| {
				let mut input_type = item.item_type;
				if proc.kind == ProcedureKind::Instrument {
					// Seen from the outside, all instrument inputs are dynamic.
					input_type.scope = Some(Scope::Dynamic);
				}
				input_type
			}).collect();
			let output_types = proc.outputs.items.iter().map(|item| item.item_type).collect();
			let signature = FullSignature {
				context: proc.context,
				kind: proc.kind,
				midi_input_count: proc.channels.len(),
				inputs: input_types,
				outputs: output_types,
			};
			self.signatures.push(signature);
		}
		self.compiler.check_errors()
	}

	pub fn infer_bodies(&mut self, program: &mut Program) -> Result<(), CompileError> {
		for (proc_index, proc) in program.procedures.iter_mut().enumerate() {
			self.current_proc_index = proc_index;
			self.current_proc_name_loc = PosRange::from(&proc.name);
			self.callees.push(vec![]);
			self.precompiled_callees.push(vec![]);
			self.infer_body(proc)?;
			self.check_outputs(proc)?;
		}
		Ok(())
	}

	fn check_outputs(&mut self, proc: &mut Procedure) -> Result<(), CompileError> {
		for PatternItem { name: variable_name, item_type } in &proc.outputs.items {
			match self.ready.get(variable_name.text.as_str()) {
				Some(&TypeResult::Type { inferred_type }) => {
					if !inferred_type.assignable_to(item_type) {
						self.compiler.report_error(variable_name,
							format!("Expression of type{} can't be assigned to output '{}'{}.",
							inferred_type, variable_name, item_type));
					}
				},
				Some(&TypeResult::Error) => {},
				None => {
					self.compiler.report_error(variable_name,
						format!("No assignment to output '{}'.", variable_name));
				},
			}
		}

		self.compiler.check_errors()
	}

	fn infer_body(&mut self, proc: &mut Procedure) -> Result<(), CompileError> {
		// Initialize ready variable to the set of parameters.
		self.ready = self.parameters.clone();

		// Record dependencies of all statements and mark inputs and fully typed
		// variables as ready.
		// A statement can be inferred when all its dependencies are ready.
		let mut dependencies = vec![HashSet::new(); proc.body.len()];
		self.find_ready(proc.kind, &mut proc.inputs);
		for (body_index, Statement::Assign { node, exp }) in proc.body.iter_mut().enumerate() {
			self.find_dependencies(exp, &mut dependencies[body_index]);
			// Inherit types from outputs.
			for PatternItem { name, item_type } in &mut node.items {
				if let Some(output) = proc.outputs.items.iter().find(|item| item.name.text == name.text) {
					item_type.inherit(&output.item_type);
				}
			}
			self.find_ready(proc.kind, node);
		}
		self.compiler.check_errors()?;

		// Find variables on or between cycles.
		let mut on_cycle = vec![true; proc.body.len()];
		let mut dependency_count: HashMap<&str, i32> = HashMap::new();
		for dep in &dependencies {
			for name in dep {
				*dependency_count.entry(name).or_default() += 1;
			}
		}
		let mut changed = true;
		while changed {
			changed = false;
			for (body_index, Statement::Assign { node, .. }) in proc.body.iter().enumerate() {
				if on_cycle[body_index] && node.items.iter().all(|item| {
					*dependency_count.entry(item.name.text.as_str()).or_default() == 0
				}) {
					for name in &dependencies[body_index] {
						*dependency_count.entry(name).or_default() -= 1;
					}
					on_cycle[body_index] = false;
					changed = true;
				}
			}
		}

		let mut done = vec![false; proc.body.len()];
		let mut done_count = 0;
		while done_count < proc.body.len() {
			let mut changed = false;
			for (body_index, Statement::Assign { node, exp }) in proc.body.iter_mut().enumerate() {
				if !done[body_index] && dependencies[body_index].iter()
						.all(|name| self.ready.contains_key(&name.to_string())) {
					self.infer_statement(node, exp);
					done[body_index] = true;
					done_count += 1;
					changed = true;
				}
			}
			if !changed {
				// There are cycles. Require width and infer rest of type.
				for (body_index, Statement::Assign { node, .. }) in proc.body.iter_mut().enumerate() {
					if !done[body_index] && on_cycle[body_index] {
						for PatternItem { name: variable_name, item_type } in &mut node.items {
							let Type { scope, width, value_type } = item_type;
							if proc.kind != ProcedureKind::Function {
								if scope.is_none() {
									*scope = Some(Scope::Dynamic);
								}
							}
							if width.is_none() {
								self.compiler.report_error(&*variable_name,
									"Variables on a cycle must specify explict width (mono, stereo or generic).");
								*width = Some(Width::Stereo);
							}
							if value_type.is_none() {
								*value_type = Some(ValueType::Number);
							}
						}
						self.find_ready(proc.kind, node);
					}
				}
			}
		}

		self.compiler.check_errors()
	}

	fn find_ready(&mut self,
			proc_kind: ProcedureKind,
			pattern: &mut Pattern) {
		for PatternItem { name: variable_name, item_type } in &mut pattern.items {
			let Type { scope, width, value_type } = item_type;
			// Mark variable ready if it is fully typed.
			let is_ready = match proc_kind {
				ProcedureKind::Function => {
					if scope.is_some() {
						self.compiler.report_error(&*variable_name,
							"Variables in functions can't be marked static or dynamic.");
					}
					width.is_some() && value_type.is_some()
				},
				_ => scope.is_some() && width.is_some() && value_type.is_some(),
			};
			if is_ready {
				self.ready.insert(variable_name.text.clone(), TypeResult::Type {
					inferred_type: *item_type,
				});
			}
		}
	}

	fn find_dependencies(&mut self,
			exp: &Expression,
			dependencies: &mut HashSet<String>) {
		exp.traverse_pre(&mut |exp| {
			if let Expression::Variable { name: variable_name } = exp {
				match self.names.lookup_variable(self.current_proc_index, &variable_name.text) {
					Some(VariableRef { kind, .. }) => {
						match kind {
							VariableKind::Input | VariableKind::Node { .. } => {
								dependencies.insert(variable_name.text.clone());
							},
							_ => {},
						}
					},
					None => {
						self.compiler.report_error(variable_name,
							format!("Variable not found: '{}'.", variable_name));
					},
				}
			}
		});
	}

	fn infer_statement(&mut self,
			node: &mut Pattern,
			exp: &mut Expression) {
		let FullSignature { kind: current_kind, .. } = self.signatures[self.current_proc_index];
		let exp_types = self.infer_expression(exp);
		if node.items.len() != exp_types.len() {
			self.compiler.report_error(node,
				format!("Mismatching number of values: {} in pattern, {} in expression.",
					node.items.len(), exp_types.len()));
		}
		let single = node.items.len() == 1;
		let node_iter = node.items.iter_mut();
		let exp_iter = exp_types.iter().chain(repeat(&TypeResult::Error));
		let mut seen_static = false;
		let mut seen_dynamic = false;
		for (item, exp_type) in node_iter.zip(exp_iter) {
			let mut node_type = item.item_type;
			let result = match exp_type {
				TypeResult::Type { inferred_type } => {
					node_type.inherit(inferred_type);
					if current_kind != ProcedureKind::Function {
						node_type.scope = node_type.scope.or(Some(Scope::Static));
						match node_type.scope.unwrap() {
							Scope::Static => seen_static = true,
							Scope::Dynamic => seen_dynamic = true,
						}
					}
					if inferred_type.assignable_to(&node_type) {
						if let Some(width) = self.should_expand(&node_type, &exp_type, Some(Width::Generic)) {
							if single {
								self.expand(exp, width);
							} else {
								self.compiler.report_error(&item.name,
									"Values inside a tuple can't be auto-expanded from mono.");
							}
						}
						item.item_type = node_type;
						TypeResult::Type { inferred_type: node_type }
					} else {
						self.compiler.report_error(&item.name,
							format!("Expression of type{} can't be assigned to '{}'{}.",
								inferred_type, item.name, item.item_type));
						TypeResult::Error
					}
				},
				TypeResult::Error => {
					TypeResult::Error
				},
			};
			self.ready.insert(item.name.text.clone(), result);
		}
		if seen_static && seen_dynamic {
			self.compiler.report_error(node,
				"Can't have both static and dynamic in the same pattern.");
		}
	}

	fn infer_expression(&mut self,
			exp: &mut Expression) -> Vec<TypeResult> {
		let loc = &PosRange::from(exp) as &dyn Location;
		use Expression::*;
		let (results, generic_width) = match exp {
			Number { value, .. } => self.infer_number(value, loc),
			Bool { value, .. } => self.infer_bool(value, loc),
			Variable { name } => self.infer_variable(name, loc),
			UnOp { op, exp } => self.infer_unop(op, exp, loc),
			BinOp { left, op, right } => self.infer_binop(left, op, right, loc),
			Conditional { condition, then, otherwise }
				=> self.infer_conditional(condition, then, otherwise, loc),
			Call { channels, name, args, .. } => self.infer_call(channels, name, args, loc),
			Tuple { elements, .. } => self.infer_tuple(elements, loc),
			Merge { left, right, .. } => self.infer_merge(left, right, loc),
			TupleIndex { exp, index, .. } => self.infer_tuple_index(exp, *index, loc),
			BufferIndex { exp, index, .. } => self.infer_buffer_index(exp, index, loc),
			For { name, count, combinator, body, .. }
				=> self.infer_for(name, count, combinator, body, loc),
			Expand { .. } => panic!(),
		};
		if let Some(generic_width) = generic_width {
			self.store_width(exp, generic_width)
		}
		results
	}

	fn expect_single(&mut self,
			exp: &mut Expression,
			title: &str) -> TypeResult {
		let result = self.infer_expression(exp);
		if result.len() != 1 {
			self.compiler.report_error(exp, format!("Single value expected for {}.", title));
			TypeResult::Error
		} else {
			result[0]
		}
	}

	fn store_width(&mut self, exp: &Expression, width: Width) {
		self.stored_widths.insert(exp as *const Expression, width);
	}

	fn infer_number(&mut self,
			_value: &f64,
			_loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let inferred_type = type_spec!(mono number);
		(vec![TypeResult::Type { inferred_type }], None)
	}

	fn infer_bool(&mut self,
			_value: &bool,
			_loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let inferred_type = type_spec!(mono bool);
		(vec![TypeResult::Type { inferred_type }], None)
	}

	fn infer_variable(&mut self,
			name: &Id,
			_loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		(vec![self.ready[&name.text]], None)
	}

	fn infer_unop(&mut self,
			op: &UnOp, exp: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let operand = self.expect_single(exp, "operand of unary operator");
		let results = self.check_signature_expand(op.signature(), &[operand], &mut [exp], loc);
		(results, operand.width())
	}

	fn infer_binop(&mut self,
			left: &mut Expression, op: &BinOp, right: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let left_operand = self.expect_single(left, "left operand of binary operator");
		let right_operand = self.expect_single(right, "right operand of binary operator");
		let results = self.check_signature_expand(op.signature(), &[left_operand, right_operand], &mut [left, right], loc);
		(results, None)
	}

	fn infer_conditional(&mut self,
			condition: &mut Expression, then: &mut Expression, otherwise: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let condition_operand = self.expect_single(condition, "condition of conditional operator");
		let then_operand = self.expect_single(then, "operand of conditional operator");
		let otherwise_operand = self.expect_single(otherwise, "operand of conditional operator");
		let sig = sig!([generic bool, generic typeless, generic typeless] [generic typeless]);
		let results = self.check_signature_expand(&sig,
			&[condition_operand, then_operand, otherwise_operand],
			&mut [condition, then, otherwise],
			loc);
		if let TypeResult::Type {
			inferred_type: Type { value_type: Some(ValueType::Buffer), .. }
		} = results[0] {
			if let TypeResult::Type {
				inferred_type: Type { width, .. }
			} = condition_operand {
				if width != Some(Width::Mono) {
					self.compiler.report_error(loc,
						"Conditionals on buffers must have mono conditions.");
					return (vec![TypeResult::Error], None);
				}
			}
		}
		(results, None)
	}

	fn infer_call(&mut self,
			channels: &Vec<MidiChannel>, name: &Id, args: &mut Vec<Expression>,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		match self.names.lookup_procedure(&name.text) {
			Some(ProcedureRef { context, kind, definition }) => {
				let FullSignature {
					context: current_context, kind: current_kind, ..
				} = self.signatures[self.current_proc_index];
				let channels_loc = &(loc.pos_before(), name.before);
				use Context::*;
				use ProcedureKind::*;
				match (channels.len(), context, kind, current_context, current_kind) {
					(_, _, Module, _, Function) => {
						self.compiler.report_error(name,
							"Modules can't be called from functions.");
					},
					(_, Global, Module, Global, _) => {},
					(_, Global, Module, _, _) => {
						self.compiler.report_error(name,
							"Global modules can only be called from other global modules.");
					},
					(1.., _, Module, _, _) => {
						self.compiler.report_error(channels_loc,
							"Only global modules can be prefixed with midi channels.");
					},
					(_, Note, Module, Note, _) => {},
					(_, Note, Module, _, _) => {
						self.compiler.report_error(name,
							"Note modules can only be called from instruments and other note modules.");
					},
					(_, _, Module, _, _) => {},
					(1.., _, Function, _, _) => {
						self.compiler.report_error(channels_loc,
							"Functions can't be prefixed with midi channels.");
					},
					(_, Global, Function, Global, _) => {},
					(_, Global, Function, _, _) => {
						self.compiler.report_error(name,
							"Global functions can only be called from global modules and other global functions.");
					},
					(_, Note, Function, Note, _) => {},
					(_, Note, Function, _, _) => {
						self.compiler.report_error(name,
							"Note functions can only be called from instruments, note modules and other note functions.");
					},
					(_, _, Function, _, _) => {},
					(1, _, Instrument, Global, Module) => {},
					(0, _, Instrument, _, _) => {
						self.compiler.report_error(channels_loc,
							"Instruments must be prefixed with a midi channel and '::'.");
					},
					(2.., _, Instrument, _, _) => {
						self.compiler.report_error(channels_loc,
							"Instruments only take a single midi channel input.");
					},
					(_, _, Instrument, _, _) => {
						self.compiler.report_error(name,
							"Instruments can only be called from global modules.");
					},
				}
				match definition {
					ProcedureDefinition::BuiltIn { sig, .. } => {
						self.check_call_signature(sig, args, loc)
					}
					ProcedureDefinition::Precompiled { proc } => {
						self.precompiled_callees[self.current_proc_index].push(*proc);
						self.check_call_signature(&proc.signature(), args, loc)
					}
					ProcedureDefinition::Declaration { proc_index } => {
						self.callees[self.current_proc_index].push(*proc_index);
						let FullSignature {
							midi_input_count, inputs, outputs, ..
						} = &self.signatures[*proc_index].clone();
						if *kind == Instrument || channels.len() == *midi_input_count {
							for channel in channels {
								match channel {
									MidiChannel::Value { channel } => {
										if *channel < 1 || *channel > 16 {
											self.compiler.report_error(channels_loc,
												"Midi channel must be between 1 and 16.");
										}
									},
									MidiChannel::Named { name } => {
										if let None = self.names.lookup_midi_input(self.current_proc_index, &name.text) {
											self.compiler.report_error(name,
												format!("Midi channel input not found: '{}'.", name));
											self.compiler.report_context(&self.current_proc_name_loc,
												"Declare midi channel inputs in front of the module name, separated by '::'.");
										}
									}
								}
							}
						} else {
							self.compiler.report_error(name,
								format!("Incorrect number of midi channels: {} given, {} expected",
									channels.len(), midi_input_count));
						}
						let sig = &Signature { inputs, outputs };
						self.check_call_signature(sig, args, loc)
					},
				}
			},
			None => {
				self.compiler.report_error(name, format!("Procedure not found: '{}'.", name));
				(vec![TypeResult::Error], None)
			},
		}
	}

	fn infer_tuple(&mut self,
			elements: &mut Vec<Expression>,
			_loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let mut results = vec![];
		for exp in elements {
			results.append(&mut self.infer_expression(exp));
		}
		(results, None)
	}

	fn infer_merge(&mut self,
			left: &mut Expression, right: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let left_operand = self.expect_single(left, "left operand of merge");
		let right_operand = self.expect_single(right, "right operand of merge");
		let merge_sig = sig!([mono typeless, mono typeless] [stereo typeless]);
		let results = self.check_signature(&merge_sig, &[left_operand, right_operand], loc);
		(results, None)
	}

	fn infer_tuple_index(&mut self,
			exp: &mut Expression, index: u64,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let tuple = self.infer_expression(exp);
		let index = index as usize;
		if index < tuple.len() {
			(vec![tuple[index]], None)
		} else {
			self.compiler.report_error(loc,
				format!("Index {} outside range of tuple of length {}.", index, tuple.len()));
			(vec![TypeResult::Error], None)
		}
	}

	fn infer_buffer_index(&mut self,
			exp: &mut Expression, index: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let buffer_operand = self.expect_single(exp, "buffer operand of indexing");
		let index_operand = self.expect_single(index, "index operand of indexing");
		let index_sig = sig!([generic buffer, mono number] [generic number]);
		let results = self.check_signature(&index_sig, &[buffer_operand, index_operand], loc);
		(results, None)
	}

	fn infer_for(&mut self,
			name: &Id, count: &mut Expression,
			combinator: &Id, body: &mut Expression,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		if let None = self.names.lookup_combinator(&combinator.text) {
			self.compiler.report_error(combinator,
				format!("Permitted repetition combinators are {}", self.names.combinator_list()));
		}
		let count_operand = self.expect_single(count, "repetition count");
		self.ready.insert(name.text.clone(), TypeResult::Type {
			inferred_type: type_spec!(static mono number),
		});
		let body_operand = self.expect_single(body, "body of repetition");
		self.ready.remove(&name.text);
		let for_sig = sig!([static mono number, dynamic generic number] [dynamic generic number]);
		let results = self.check_signature(&for_sig, &[count_operand, body_operand], loc);
		(results, body_operand.width())
	}

	fn check_call_signature(&mut self, sig: &Signature, args: &mut Vec<Expression>,
			loc: &dyn Location) -> (Vec<TypeResult>, Option<Width>) {
		let mut arg_types = vec![];
		let mut arg_multiplicity = vec![];
		for arg in args.iter_mut() {
			let mut arg_result = self.infer_expression(arg);
			arg_multiplicity.push(arg_result.len());
			arg_types.append(&mut arg_result);
		}
		if sig.inputs.len() == arg_types.len() {
			let result = self.check_signature(sig, &arg_types, loc);
			let generic_width = self.generic_instantiation(sig, &result);
			let mut type_index = 0;
			for (arg, multiplicity) in args.iter_mut().zip(arg_multiplicity) {
				for i in type_index .. type_index + multiplicity {
					if let Some(width) = self.should_expand(&sig.inputs[i], &arg_types[i], generic_width) {
						if multiplicity == 1 {
							self.expand(arg, width);
						} else {
							self.compiler.report_error(loc,
								"Values inside a tuple can't be auto-expanded from mono.");
						}
					}
				}
				type_index += multiplicity;
			}
			(result, generic_width)
		} else {
			self.compiler.report_error(loc,
				format!("{} arguments expected, {} given.", sig.inputs.len(), arg_types.len()));
			(vec![TypeResult::Error; sig.outputs.len()], None)
		}
	}

	fn check_signature_expand(&mut self, sig: &Signature, args: &[TypeResult],
			inputs: &mut [&mut Expression], loc: &dyn Location) -> Vec<TypeResult> {
		let result = self.check_signature(sig, args, loc);
		let generic_width = self.generic_instantiation(sig, &result);
		for ((sig_type, arg_type), input) in sig.inputs.iter().zip(args).zip(inputs) {
			if let Some(width) = self.should_expand(sig_type, arg_type, generic_width) {
				self.expand(input, width);
			}
		}
		result
	}

	fn generic_instantiation(&self, sig: &Signature, result: &Vec<TypeResult>) -> Option<Width> {
		let mut generic = false;
		for (sig_out, result_out) in sig.outputs.iter().zip(result) {
			if let (Type {
				width: Some(sig_width), ..
			},
			TypeResult::Type {
				inferred_type: Type {
					width: Some(result_width), ..
				}
			}) = (sig_out, result_out) {
				match (sig_width, result_width) {
					(Width::Generic, Width::Mono) => { generic = true; },
					(Width::Generic, Width::Stereo) => return Some(Width::Stereo),
					(Width::Generic, Width::Generic) => return Some(Width::Generic),
					_ => {},
				}
			}
		}
		generic.then_some(Width::Mono)
	}

	fn should_expand(&self, sig_type: &Type, arg_type: &TypeResult, generic_width: Option<Width>) -> Option<Width> {
		if let (Type {
			width: Some(sig_width), ..
		},
		TypeResult::Type {
			inferred_type: Type {
				width: Some(arg_width), ..
			}
		}) = (sig_type, arg_type) {
			match (sig_width, arg_width, generic_width) {
				(Width::Stereo, Width::Mono, _) => Some(Width::Stereo),
				(Width::Generic, Width::Mono, Some(Width::Stereo)) => Some(Width::Stereo),
				(Width::Generic, Width::Mono, Some(Width::Generic)) => Some(Width::Generic),
				_ => None,
			}
		} else {
			None
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
						(Width::Mono, Width::Mono) => {},
						(s, Width::Mono) => if arg_type.value_type == Some(ValueType::Buffer) {
							self.compiler.report_error(loc,
								format!("Can't pass a mono buffer value into a {} buffer input.", s));
						},
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

	fn expand(&mut self, exp: &mut Expression, width: Width) {
		let dummy = Expression::Bool {
			before: exp.pos_before(),
			value: true,
		};
		let node = replace(exp, dummy);
		*exp = Expression::Expand {
			exp: Box::new(node),
			width,
		};
		let key = exp as *const Expression;
		if let Some(width) = self.stored_widths.get(&key) {
			if let Expression::Expand { exp: inner, .. } = exp {
				self.store_width(inner, *width);
				self.stored_widths.remove(&key);
			}
		}
	}
}
