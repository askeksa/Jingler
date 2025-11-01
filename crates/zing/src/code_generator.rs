
use std::collections::HashMap;
use std::mem::{replace, take};

use program::program::*;
use program::instructions::*;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::*;
use crate::names::*;

pub fn generate_code<'ast, 'input, 'comp>(
		program: &'ast Program<'ast>,
		names: &'ast Names<'ast>,
		signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
		stored_widths: HashMap<*const Expression<'ast>, Width>,
		callees: Vec<Vec<usize>>,
		compiler: &mut Compiler<'input>)
-> Result<(Vec<ZingProcedure>, usize, usize, Vec<usize>), CompileError> {
	let mut cg = CodeGenerator::new(names, compiler, signatures, stored_widths, callees);
	cg.generate_code_for_program(program)?;
	let main_index = match names.lookup_procedure("main").unwrap().definition {
		ProcedureDefinition::Declaration { proc_index } => proc_index,
		_ => panic!("No main"),
	};
	let main_static_proc_id = cg.static_proc_id[main_index] as usize;
	let main_dynamic_proc_id = cg.dynamic_proc_id[main_index] as usize;
	Ok((take(&mut cg.procedures), main_static_proc_id, main_dynamic_proc_id, take(&mut cg.track_order)))
}

fn statement_scope<'ast>(statement: &Statement<'ast>) -> Option<Scope> {
	let Statement::Assign { node, .. } = statement;
	node.items.first().and_then(|item| item.item_type.scope)
}


#[derive(Clone, Copy)]
enum StateKind { Cell, Delay }

#[derive(Clone)]
enum ModuleCall<'ast> {
	Init {
		kind: StateKind,
		value: &'ast Expression<'ast>,
		width: ZingWidth,
	},
	Call {
		proc_index: usize,
		generic_width: Option<ZingWidth>,
		args: &'ast Vec<Expression<'ast>>,
	},
	For {
		name: &'ast Id<'ast>,
		count: &'ast Expression<'ast>,
		nested_calls: Vec<ModuleCall<'ast>>,
	},
}

impl From<Width> for ZingWidth {
	fn from(width: Width) -> Self {
		match width {
			Width::Mono => ZingWidth::Mono,
			Width::Stereo => ZingWidth::Stereo,
			Width::Generic => ZingWidth::Generic,
		}
	}
}

impl From<ValueType> for ZingValueType {
	fn from(value_type: ValueType) -> Self {
		match value_type {
			ValueType::Number => ZingValueType::Number,
			ValueType::Bool => ZingValueType::Number,
			ValueType::Buffer => ZingValueType::Buffer,
			ValueType::Typeless => panic!("Typeless parameter in signature"),
		}
	}
}

struct CodeGenerator<'ast, 'input, 'comp> {
	names: &'ast Names<'ast>,
	compiler: &'comp mut Compiler<'input>,
	signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
	stored_widths: HashMap<*const Expression<'ast>, Width>,
	callees: Vec<Vec<usize>>,

	/// Is the procedure reachable from main?
	live: Vec<bool>,
	/// Procedure ID for functions
	function_proc_id: Vec<u16>,
	/// Static procedure ID for modules and instruments
	static_proc_id: Vec<u16>,
	/// Dynamic procedure ID for modules and instruments
	dynamic_proc_id: Vec<u16>,
	// Procedure index and scope for ID
	proc_for_id: Vec<(usize, Option<Scope>)>,
	// The current procedure kind
	current_kind: ProcedureKind,

	procedures: Vec<ZingProcedure>,
	code: Vec<Instruction>,
	stack_height: usize,

	/// Next stack index
	next_stack_index: usize,
	/// Stack index of variable
	stack_index: HashMap<&'ast str, usize>,
	/// Stack index of static variable in implicit cell
	stack_index_in_cell: Vec<usize>,
	// Variable names of implicit cells
	name_in_cell: HashMap<usize, &'ast str>,
	// Nesting depth of repetitions
	repetition_depth: usize,
	// Module calls in execution order
	module_call: Vec<ModuleCall<'ast>>,
	// Tracks in execution order
	track_order: Vec<usize>,
	// Queue for dynamic cell update expressions
	update_stack: Vec<(StateKind, &'ast Expression<'ast>)>,
}

impl<'ast, 'input, 'comp> CodeGenerator<'ast, 'input, 'comp> {
	pub fn new(
			names: &'ast Names<'ast>,
			compiler: &'comp mut Compiler<'input>,
			signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
			stored_widths: HashMap<*const Expression<'ast>, Width>,
			callees: Vec<Vec<usize>>)
			-> CodeGenerator<'ast, 'input, 'comp> {
		let proc_count = callees.len();
		CodeGenerator {
			names,
			compiler,
			signatures,
			stored_widths,
			callees,

			live: vec![false; proc_count],
			function_proc_id: vec![0; proc_count],
			static_proc_id: vec![0; proc_count],
			dynamic_proc_id: vec![0; proc_count],
			proc_for_id: vec![],
			current_kind: ProcedureKind::Module,

			procedures: vec![],
			code: vec![],
			stack_height: 0,

			next_stack_index: 0,
			stack_index: HashMap::new(),
			stack_index_in_cell: vec![],
			name_in_cell: HashMap::new(),
			repetition_depth: 0,
			module_call: vec![],
			track_order: vec![],
			update_stack: vec![],
		}
	}

	fn retrieve_width(&self, exp: &Expression<'ast>) -> Option<ZingWidth> {
		self.stored_widths.get(&(exp as *const Expression<'ast>)).map(|&w| w.into())
	}

	fn unsupported(&mut self, loc: &dyn Location, what: &str) {
		self.compiler.report_error(loc, format!("Not supported yet: {}.", what));
	}

	fn emit(&mut self, code: &[Instruction]) {
		for &inst in code {
			let (popped, pushed) = match inst {
				Instruction::Call(id, ..) => {
					let (proc_index, scope) = self.proc_for_id[id as usize];
					let (kind, inputs, outputs) = &self.signatures[proc_index];
					match kind {
						ProcedureKind::Module => {
							if scope == Some(Scope::Static) {
								let input_count = inputs.iter()
									.filter(|t| t.scope == Some(Scope::Static)).count();
								(input_count, 0)
							} else {
								let input_count = inputs.iter()
									.filter(|t| t.scope == Some(Scope::Dynamic)).count();
								(input_count, outputs.len())
							}
						},
						ProcedureKind::Function => {
							(inputs.len(), outputs.len())
						},
						ProcedureKind::Instrument => {
							(inputs.len() + 1, inputs.len() + 1)
						},
					}
				},
				_ => inst.stack_change(),
			};
			//println!("{:4}  {:2}  {:?}", self.code.len(), self.stack_height, inst);
			self.code.push(inst);
			if popped > self.stack_height {
				panic!("Stack underflow: {:?}", self.code);
			}
			self.stack_height -= popped;
			self.stack_height += pushed;
		}
	}

	pub fn generate_code_for_program(&mut self, program: &'ast Program<'ast>) -> Result<(), CompileError> {
		self.propagate_liveness(program.procedures.iter().position(|p| p.name.text == "main").unwrap());
		self.init_proc_id(program);
		for id in 0..self.proc_for_id.len() {
			let (proc_index, scope) = self.proc_for_id[id];
			let Procedure { kind, name, inputs, outputs, body, .. } = &program.procedures[proc_index];
			self.current_kind = *kind;
			let proc_kind;
			let proc_inputs;
			let proc_outputs;
			match kind {
				ProcedureKind::Module => {
					if scope == Some(Scope::Static) {
						proc_kind = ZingProcedureKind::Module { scope: ZingScope::Static };
						proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Static));
						proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Static));
						self.initialize_stack(inputs, Some(Scope::Static), false);
						self.find_cells_in_body(body)?;
						self.mark_implicit_cells_from_outputs(outputs);
						self.initialize_stack(inputs, Some(Scope::Static), false);
						self.generate_static_body(body)?;
						self.adjust_stack(&[], self.stack_height);
					} else {
						proc_kind = ZingProcedureKind::Module { scope: ZingScope::Dynamic };
						proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Dynamic));
						proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Dynamic));
						self.initialize_stack(inputs, Some(Scope::Dynamic), false);
						self.generate_dynamic_body(body)?;
						let stack_adjust = self.stack_adjust_from_outputs(outputs);
						self.adjust_stack(&stack_adjust[..], self.stack_height);
					}
				},
				ProcedureKind::Function => {
					proc_kind = ZingProcedureKind::Function;
					proc_inputs = self.make_proc_type_list(inputs, None);
					proc_outputs = self.make_proc_type_list(outputs, None);
					self.initialize_stack(inputs, None, false);
					for statement in body {
						self.generate_code_for_statement(statement)?;
					}
					let stack_adjust = self.stack_adjust_from_outputs(outputs);
					self.adjust_stack(&stack_adjust[..], self.stack_height);
				},
				ProcedureKind::Instrument => {
					// Extra implicit input for accumulating instrument outputs.
					let mut real_inputs = inputs.clone();
					real_inputs.items.push(PatternItem {
						name: Id {
							text: "#acc#", before: outputs.before,
						},
						item_type: type_spec!(dynamic stereo number),
					});
					if scope == Some(Scope::Static) {
						proc_kind = ZingProcedureKind::Instrument { scope: ZingScope::Static };
						self.initialize_stack(&real_inputs, Some(Scope::Static), true);
						self.find_cells_in_body(body)?;
						self.mark_implicit_cells_from_outputs(outputs);
						self.initialize_stack(&real_inputs, Some(Scope::Static), true);
						self.generate_static_body(body)?;
						// Init autokill
						self.emit(code![
							Constant(0),
							CellInit
						]);
						// Leave only the inputs (including the accumulator) on the stack.
						self.adjust_stack(&[], self.stack_height - real_inputs.items.len());
					} else {
						proc_kind = ZingProcedureKind::Instrument { scope: ZingScope::Dynamic };
						self.initialize_stack(&real_inputs, Some(Scope::Dynamic), true);
						self.generate_dynamic_body(body)?;
						// Run autokill code
						self.emit(code![
							Constant(0x46000000), // Counter threshold
							StackLoad(1), // Copy of output
							Constant(0x46000000), ExpandStereo, Mul, Round, // 0 when small
							SplitRL, Or, // 0 when both channels small
							Constant(0), Eq, // True when small
							CellPush, And, // Preserve counter when small
							Constant(0x3F800000), Add, // Increment counter
							StackLoad(0), CellPop, // Update counter
							IfGreaterEq, // Has counter reached threshold?
							Kill, // Kill when counter reaches threshold
							EndIf, Pop, Pop
						]);
						// Leave the inputs (including the accumulator) and the output on the stack.
						let mut stack_adjust: Vec<usize> = (0..(inputs.items.len() + 1)).collect();
						stack_adjust.push(self.stack_index[outputs.items[0].name.text]);
						self.adjust_stack(&stack_adjust, self.stack_height);
						// Add the output to the accumulator.
						self.emit(code![Add]);
					}
					proc_inputs = self.make_proc_type_list(inputs, None);
					proc_outputs = self.make_proc_type_list(inputs, None);
				},
			}
			self.procedures.push(ZingProcedure {
				name: name.text.into(),
				kind: proc_kind,
				inputs: proc_inputs,
				outputs: proc_outputs,
				code: take(&mut self.code),
			});
		}

		self.compiler.check_errors()
	}

	fn propagate_liveness(&mut self, proc_index: usize) {
		if !self.live[proc_index] {
			self.live[proc_index] = true;
			for &callee in &self.callees[proc_index].clone() {
				self.propagate_liveness(callee);
			}
		}
	}

	fn init_proc_id(&mut self, program: &Program<'ast>) {
		// Assign ID to all procedures in this order:
		// main, instruments, modules (except main), functions.
		let mut assign_ids = |pred: &dyn Fn(ProcedureKind, &str) -> bool| {
			for (proc_index, proc) in program.procedures.iter().enumerate() {
				if self.live[proc_index] && pred(proc.kind, proc.name.text) {
					let mut push_id = |proc_id: &mut Vec<u16>, scope: Option<Scope>| {
						proc_id[proc_index] = self.proc_for_id.len() as u16;
						self.proc_for_id.push((proc_index, scope));
					};
					match proc.kind {
						ProcedureKind::Function => {
							push_id(&mut self.function_proc_id, None);
						},
						ProcedureKind::Module | ProcedureKind::Instrument => {
							push_id(&mut self.static_proc_id, Some(Scope::Static));
							push_id(&mut self.dynamic_proc_id, Some(Scope::Dynamic));
						},
					};
				}
			}
		};
		assign_ids(&|_, name| name == "main");
		assign_ids(&|kind, _| kind == ProcedureKind::Instrument);
		assign_ids(&|kind, name| kind == ProcedureKind::Module && name != "main");
		assign_ids(&|kind, _| kind == ProcedureKind::Function);
	}

	fn generate_static_body(&mut self, body: &'ast Vec<Statement<'ast>>) -> Result<(), CompileError> {
		for statement in body {
			if statement_scope(statement) == Some(Scope::Static) {
				self.generate_code_for_statement(statement)?;
			}
		}
		for (cell_index, stack_index) in self.stack_index_in_cell.clone().iter().enumerate() {
			let offset = self.stack_height - stack_index - 1;
			self.emit(code![StackLoad(offset as u16), CellInit]);
			if let Some((name, _)) = self.stack_index.iter().find(|(_, i)| *i == stack_index) {
				self.name_in_cell.insert(cell_index, name);
			}
		}
		self.generate_static_module_calls(&self.module_call.clone());
		Ok(())
	}

	fn generate_static_module_calls(&mut self, module_call: &Vec<ModuleCall<'ast>>) {
		for call in module_call {
			match call {
				&ModuleCall::Init { kind, value, width } => {
					self.generate(value);
					match kind {
						StateKind::Cell => {
							self.emit(code![CellInit]);
						},
						StateKind::Delay => {
							self.emit(code![BufferAlloc(width), CellInit]);
						},
					}
				},
				&ModuleCall::Call { proc_index, generic_width, args } => {
					let (_, inputs, _) = &self.signatures[proc_index];
					for (input_type, arg) in inputs.clone().iter().zip(args) {
						if input_type.scope == Some(Scope::Static) {
							self.generate(arg);
						}
					}
					self.emit(code![Call(self.static_proc_id[proc_index], generic_width)]);
				},
				ModuleCall::For { name, count, nested_calls } => {
					self.generate(count);
					self.emit(code![RepeatInit]);
					let counter_stack_index = self.stack_height - 1;
					self.stack_index.insert(name.text, counter_stack_index);
					self.generate_static_module_calls(nested_calls);
					self.stack_index.remove(name.text);
					self.emit(code![RepeatEnd]);
				},
			}
		}
	}

	fn generate_dynamic_body(&mut self, body: &'ast Vec<Statement<'ast>>) -> Result<(), CompileError> {
		for cell_index in 0 .. self.stack_index_in_cell.len() {
			if let Some(name) = self.name_in_cell.get(&cell_index) {
				self.stack_index.entry(name).or_insert(self.stack_height);
			}
			self.emit(code![CellRead]);
		}

		self.next_stack_index = self.stack_height;
		for statement in body {
			if statement_scope(statement) == Some(Scope::Dynamic) {
				self.generate_code_for_statement(statement)?;
			}
		}
		self.generate_dynamic_body_flush_update_stack(0);
		Ok(())
	}

	fn generate_dynamic_body_flush_update_stack(&mut self, height: usize) {
		while self.update_stack.len() > height {
			let (kind, exp) = self.update_stack.pop().unwrap();
			let base_height = self.update_stack.len();
			match kind {
				StateKind::Cell => {
					self.generate(exp);
				},
				StateKind::Delay => {
					self.emit(code![CellFetch]);
					self.generate(exp);
					self.emit(code![BufferStoreAndStep]);
				},
			}
			self.generate_dynamic_body_flush_update_stack(base_height);
			self.emit(code![CellPop]);
		}
	}

	fn generate_code_for_statement(&mut self, statement: &'ast Statement<'ast>) -> Result<(), CompileError> {
		let Statement::Assign { node, exp } = statement;
		self.generate(exp);
		self.add_stack_indices(node, None, false, false);
		self.compiler.check_errors()
	}

	fn initialize_stack(&mut self, inputs: &Pattern<'ast>, scope: Option<Scope>, all_scopes: bool) {
		self.stack_index.clear();
		self.next_stack_index = 0;
		self.stack_height = 0;
		self.add_stack_indices(inputs, scope, true, all_scopes);
	}

	fn add_stack_indices(&mut self, pattern: &Pattern<'ast>, scope: Option<Scope>, adjust_stack: bool, all_scopes: bool) {
		for item in &pattern.items {
			if (scope.is_none() || item.item_type.scope == scope) && item.name.text != "_" {
				self.stack_index.insert(item.name.text, self.next_stack_index);
			}
			if scope.is_none() || item.item_type.scope == scope || all_scopes {
				self.next_stack_index += 1;
				if adjust_stack {
					self.stack_height += 1;
				}
			}
		}
	}

	fn make_proc_type_list(&self, types: &Pattern, scope: Option<Scope>) -> Vec<ZingType> {
		let mut result = vec![];
		for PatternItem { item_type, .. } in &types.items {
			if scope.is_none() || item_type.scope == scope {
				result.push(ZingType {
					width: item_type.width.unwrap().into(),
					value_type:  item_type.value_type.unwrap().into(),
				});
			}
		}
		result
	}

	fn find_cells_in_body(&mut self, body: &'ast Vec<Statement<'ast>>) -> Result<(), CompileError> {
		self.stack_index_in_cell.clear();
		self.module_call.clear();
		debug_assert!(self.update_stack.is_empty());

		for Statement::Assign { node, .. } in body {
			self.add_stack_indices(node, Some(Scope::Static), false, false);
		}

		for Statement::Assign { node, exp } in body {
			if node.items.iter().any(|item| item.item_type.scope == Some(Scope::Dynamic)) {
				self.find_cells(exp);
			}
		}
		self.find_cells_in_body_flush_update_stack(0);

		self.compiler.check_errors()
	}

	fn find_cells_in_body_flush_update_stack(&mut self, height: usize) {
		while self.update_stack.len() > height {
			let (_kind, exp) = self.update_stack.pop().unwrap();
			let base_height = self.update_stack.len();
			self.find_cells(exp);
			self.find_cells_in_body_flush_update_stack(base_height);
		}
	}

	fn mark_implicit_cells_from_outputs(&mut self, outputs: &'ast Pattern<'ast>) {
		for item in &outputs.items {
			self.mark_implicit_cell(&item.name);
		}
	}

	fn mark_implicit_cell(&mut self, name: &'ast Id<'ast>) {
		if let Some(index) = self.stack_index.get(name.text) {
			// Static variable
			if let None = self.stack_index_in_cell.iter().find(|&i| i == index) {
				self.stack_index_in_cell.push(*index);
			}
		}
	}

	fn find_cells(&mut self, exp: &'ast Expression<'ast>) {
		use Expression::*;
		match exp {
			Number { .. } => {},
			Bool { .. } => {},
			Variable { name } => {
				self.mark_implicit_cell(name);
			},
			UnOp { exp, .. } => {
				self.find_cells(exp);
			},
			BinOp { left, right, .. } => {
				self.find_cells(right);
				self.find_cells(left);
			},
			Conditional { condition, then, otherwise } => {
				self.find_cells(condition);
				self.find_cells(then);
				self.find_cells(otherwise);
			},
			Call { channel, name, args, .. } => {
				match self.names.lookup_procedure(name.text) {
					Some(ProcedureRef { kind, definition }) => {
						use ProcedureKind::*;
						use ProcedureDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								if self.repetition_depth > 0 {
									self.unsupported(exp, "Built-in module in repetition body");
								}
								let width = self.retrieve_width(exp).unwrap();
								match name.text {
									"cell" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Cell, value: &args[0], width });
										self.update_stack.push((StateKind::Cell, &args[1]));
									},
									"delay" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[0], width });
										self.update_stack.push((StateKind::Delay, &args[1]));
									},
									"dyndelay" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[0], width });
										self.find_cells(&args[1]);
										self.update_stack.push((StateKind::Delay, &args[2]));
									},
									_ => panic!("Unknown built-in module"),
								}
							},
							(Module, Declaration { proc_index }) => {
								let (_, inputs, _) = &self.signatures[*proc_index];
								for (arg, input_type) in args.iter().zip(inputs.clone()) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.find_cells(arg);
									}
								}
								self.module_call.push(ModuleCall::Call {
									proc_index: *proc_index,
									generic_width: self.retrieve_width(exp),
									args,
								});
							},
							(Function, BuiltIn { .. }) => {
								for arg in args {
									self.find_cells(arg);
								}
							},
							(Function, Declaration { .. }) => {
								for arg in args {
									self.find_cells(arg);
								}
							},
							(Instrument, BuiltIn { .. }) => panic!("Built-in instrument"),
							(Instrument, Declaration { .. }) => {
								self.track_order.push(channel.unwrap() - 1);
								for arg in args {
									self.find_cells(arg);
								}
							},
						}
					},
					None => panic!("Procedure not found"),
				}
			},
			Tuple { elements, ..} => {
				for element in elements {
					self.find_cells(element);
				}
			},
			Merge { left, right, .. } => {
				self.find_cells(right);
				self.find_cells(left);
			},
			Property { exp, .. } => {
				self.find_cells(exp);
			},
			TupleIndex { .. } => {
				self.unsupported(exp, "tuple indexing");
			},
			BufferIndex { exp, index, .. } => {
				self.find_cells(exp);
				self.find_cells(index);
			},
			For { name, count, body, .. } => {
				let module_call_temp = replace(&mut self.module_call, vec![]);
				self.repetition_depth += 1;
				self.find_cells(body);
				self.repetition_depth -= 1;
				let module_call = ModuleCall::For {
					name, count, nested_calls: replace(&mut self.module_call, module_call_temp)
				};
				self.module_call.push(module_call);
			},
			Expand { exp, .. } => {
				self.find_cells(exp);
			}
		}
	}

	fn stack_adjust_from_outputs(&mut self, outputs: &'ast Pattern<'ast>) -> Vec<usize> {
		let mut stack_adjust = vec![];
		for item in &outputs.items {
			let index = self.stack_index[item.name.text];
			stack_adjust.push(index);
		}
		stack_adjust
	}

	fn adjust_stack(&mut self, results: &[usize], mut height: usize) {
		match results.len() {
			0 => {
				for _ in 0..height {
					self.emit(code![Pop]);
				}
			},
			1 => {
				for _ in 0..(height - results[0] - 1) {
					self.emit(code![Pop]);
				}
				for _ in 0..results[0] {
					self.emit(code![PopNext]);
				}
			},
			_ => {
				for i in 0..results.len() {
					if results[i] != i {
						let offset = height - results[i] - 1;
						self.emit(code![StackLoad(offset as u16)]);
						height += 1;
					}
				}
				for i in (0..results.len()).rev() {
					if results[i] != i {
						height -= 1;
						let offset = height - i - 1;
						self.emit(code![StackStore(offset as u16)]);
					}
				}
				for _ in 0..(height - results.len()) {
					self.emit(code![Pop]);
				}
			},
		}
	}

	fn generate(&mut self, exp: &'ast Expression<'ast>) {
		use Expression::*;
		match exp {
			Number { value, .. } => self.emit(code![Constant((*value as f32).to_bits())]),
			Bool { value, .. } => if *value {
				self.emit(code![Constant(0), Constant(0), Eq]);
			} else {
				self.emit(code![Constant(0)]);
			},
			Variable { name } => {
				match self.stack_index.get(name.text) {
					Some(stack_index) => {
						let offset = self.stack_height - stack_index - 1;
						self.emit(code![StackLoad(offset as u16)]);
					}
					None => {
						if let Some(index) = self.names.lookup_parameter(name.text) {
							self.emit(code![Parameter(index as u16)]);
						} else {
							self.unsupported(exp, "accessing a variable above its definition (except in cell or delay)");
						}
					},
				};
			},
			UnOp { op, exp: operand } => {
				self.generate(operand);
				self.emit(code![Constant(0)]);
				self.expand(self.retrieve_width(exp).unwrap());
				self.emit(op.instructions());
			},
			BinOp { left, op, right } => {
				self.generate(right);
				self.generate(left);
				self.emit(op.instructions());
			},
			Conditional { condition, then, otherwise } => {
				self.generate(condition);
				self.generate(then);
				self.emit(code![StackLoad(1), And]);
				self.generate(otherwise);
				self.emit(code![StackLoad(2), AndNot, Or, PopNext]);
			},
			Call { name, args, .. } => {
				match self.names.lookup_procedure(name.text) {
					Some(ProcedureRef { kind, definition }) => {
						use ProcedureKind::*;
						use ProcedureDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								match name.text {
									"cell" => {
										self.emit(code![CellPush]);
										self.update_stack.push((StateKind::Cell, &args[1]));
									},
									"delay" => {
										self.emit(code![CellPush, BufferLoad]);
										self.update_stack.push((StateKind::Delay, &args[1]));
									},
									"dyndelay" => {
										self.emit(code![CellPush]);
										self.generate(&args[1]);
										self.emit(code![BufferLoadWithOffset]);
										self.update_stack.push((StateKind::Delay, &args[2]));
									},
									_ => panic!("Unknown built-in module"),
								}
							},
							(Module, Declaration { proc_index }) => {
								let (_, inputs, _) = &self.signatures[*proc_index];
								for (arg, input_type) in args.iter().zip(inputs.clone()) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.generate(arg);
									}
								}
								let proc_id = self.dynamic_proc_id[*proc_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp))]);
							},
							(Function, BuiltIn { code, .. }) => {
								let is_note_property = code.iter().any(|c| matches!(c, Instruction::ReadNoteProperty(_)));
								if is_note_property && self.current_kind != ProcedureKind::Instrument {
									self.unsupported(exp, "Note property outside instrument");
								}
								for arg in args {
									self.generate(arg);
								}
								self.emit(code);
							},
							(Function, Declaration { proc_index }) => {
								for arg in args {
									self.generate(arg);
								}
								let proc_id = self.function_proc_id[*proc_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp))]);
							},
							(Instrument, BuiltIn { .. }) => panic!("Built-in instrument"),
							(Instrument, Declaration { proc_index }) => {
								let static_proc_id = self.static_proc_id[*proc_index];
								let dynamic_proc_id = self.dynamic_proc_id[*proc_index];

								let (_, inputs, outputs) = &self.signatures[*proc_index];
								let (in_count, out_count) = (inputs.len() + 1, outputs.len());
								for arg in args {
									self.generate(arg);
								}
								self.emit(code![Constant(0), ExpandStereo]);
								self.emit(code![PlayInstrument(static_proc_id, dynamic_proc_id)]);
								let stack_adjust: Vec<usize> = (in_count - out_count .. in_count).collect();
								self.adjust_stack(&stack_adjust[..], in_count);
							},
						}
					},
					None => panic!("Procedure not found"),
				}
			},
			Tuple { elements, ..} => {
				for element in elements {
					self.generate(element);
				}
			},
			Merge { left, right, .. } => {
				self.generate(right);
				self.generate(left);
				self.emit(code![MergeLR]);
			},
			Property { exp, name } => {
				self.generate(exp);
				match name.text {
					"left" => self.emit(code![Left]),
					"right" => self.emit(code![Right]),
					"index" => self.emit(code![BufferIndex]),
					"length" => self.emit(code![BufferLength]),
					_ => panic!("Unknown property"),
				}
			},
			TupleIndex { .. } => {
				self.unsupported(exp, "tuple indexing");
			},
			BufferIndex { exp, index, .. } => {
				self.generate(exp);
				self.generate(index);
				self.emit(code![StackLoad(1), BufferIndex, Sub, BufferLoadWithOffset]);
			},
			For { name, body, combinator, .. } => {
				let combinator = self.names.lookup_combinator(combinator.text).unwrap();
				self.emit(code![Constant(combinator.neutral.to_bits())]); // accumulator
				self.expand(self.retrieve_width(exp).unwrap());
				self.emit(code![RepeatStart]);
				let counter_stack_index = self.stack_height - 1;
				self.stack_index.insert(name.text, counter_stack_index);
				self.generate(body);
				self.stack_index.remove(name.text);
				self.emit(code![StackLoad(3)]); // accumulator
				self.emit(combinator.code);
				self.emit(code![StackStore(2)]); // accumulator
				self.emit(code![RepeatEnd]);
			},
			Expand { exp, width } => {
				self.generate(exp);
				self.expand((*width).into());
			},
		}
	}

	fn expand(&mut self, width: ZingWidth) {
		match width {
			ZingWidth::Mono => {},
			ZingWidth::Stereo => self.emit(code![ExpandStereo]),
			ZingWidth::Generic => self.emit(code![ExpandGeneric]),
		}
	}
}
