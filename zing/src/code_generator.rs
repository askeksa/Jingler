
use std::collections::{HashMap, VecDeque};
use std::mem::{replace, take};

use program::program::*;
use program::instructions::*;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::*;
use crate::names::*;

const AUTOKILL_CELL_INIT: Expression<'static> = Expression::Number {
	before: 0, value: 0.0, after: 0
};

pub fn generate_code<'ast, 'input, 'comp>(
		program: &'ast Program<'ast>,
		names: &'ast Names<'ast>,
		signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
		stored_widths: HashMap<*const Expression<'ast>, Width>,
		compiler: &mut Compiler<'input>) -> Result<(Vec<ZingProcedure>, Vec<usize>), CompileError> {
	let mut cg = CodeGenerator::new(names, compiler, signatures, stored_widths);
	cg.generate_code_for_program(program)?;
	Ok((take(&mut cg.procedures), take(&mut cg.instrument_order)))
}

fn statement_scope<'ast>(statement: &Statement<'ast>) -> Option<Scope> {
	let Statement::Assign { node, .. } = statement;
	node.items.first().and_then(|item| item.item_type.scope)
}


#[derive(Clone)]
enum StateKind { Cell, Delay }

#[derive(Clone)]
enum ModuleCall<'ast> {
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

	/// Lowest id (of two for modules and instruments) for procedure
	proc_id: Vec<u16>,
	// Procedure index for id
	proc_for_id: Vec<usize>,
	// The id of last called instrument
	instrument_id: usize,
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
	// Stack index of next cell in execution order
	cell_stack_index: usize,
	// Nesting depth of repetitions
	repetition_depth: usize,
	// Static expression to initialize explicit cell
	cell_init: Vec<(StateKind, &'ast Expression<'ast>, ZingWidth)>,
	// Module calls in execution order
	module_call: Vec<ModuleCall<'ast>>,
	// Instruments in execution order
	instrument_order: Vec<usize>,
	// Queue for dynamic cell update expressions
	update_queue: VecDeque<(StateKind, &'ast Expression<'ast>)>,
}

impl<'ast, 'input, 'comp> CodeGenerator<'ast, 'input, 'comp> {
	pub fn new(
			names: &'ast Names<'ast>,
			compiler: &'comp mut Compiler<'input>,
			signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
			stored_widths: HashMap<*const Expression<'ast>, Width>)
			-> CodeGenerator<'ast, 'input, 'comp> {
		CodeGenerator {
			names,
			compiler,
			signatures,
			stored_widths,

			proc_id: vec![],
			proc_for_id: vec![],
			instrument_id: 0,
			current_kind: ProcedureKind::Module,

			procedures: vec![],
			code: vec![],
			stack_height: 0,

			next_stack_index: 0,
			stack_index: HashMap::new(),
			stack_index_in_cell: vec![],
			name_in_cell: HashMap::new(),
			cell_stack_index: 0,
			repetition_depth: 0,
			cell_init: vec![],
			module_call: vec![],
			instrument_order: vec![],
			update_queue: VecDeque::new(),
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
					let proc_index = self.proc_for_id[id as usize];
					let (kind, inputs, outputs) = &self.signatures[proc_index];
					let is_static = (id & 1) == 0;
					match kind {
						ProcedureKind::Module => {
							if is_static {
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
		self.init_proc_id(program);
		for id in 0..self.proc_for_id.len() {
			let proc_index = self.proc_for_id[id];
			let Procedure { kind, name, inputs, outputs, body, .. } = &program.procedures[proc_index];
			self.current_kind = *kind;
			let proc_kind;
			let proc_inputs;
			let proc_outputs;
			match kind {
				ProcedureKind::Module => {
					let is_static = (id & 1) == 0;
					if is_static {
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
					let is_static = (id & 1) == 0;
					if is_static {
						proc_kind = ZingProcedureKind::Instrument { scope: ZingScope::Static };
						self.initialize_stack(&real_inputs, Some(Scope::Static), true);
						self.find_cells_in_body(body)?;
						self.cell_init.push((StateKind::Cell, &AUTOKILL_CELL_INIT, ZingWidth::Mono));
						self.mark_implicit_cells_from_outputs(outputs);
						self.initialize_stack(&real_inputs, Some(Scope::Static), true);
						self.generate_static_body(body)?;
						// Leave only the inputs (including the accumulator) on the stack.
						self.adjust_stack(&[], self.stack_height - real_inputs.items.len());
					} else {
						proc_kind = ZingProcedureKind::Instrument { scope: ZingScope::Dynamic };
						self.initialize_stack(&real_inputs, Some(Scope::Dynamic), true);
						self.generate_dynamic_body(body)?;
						// Run autokill code
						let counter_stack_index = self.cell_stack_index;
						let counter_offset = self.stack_height - counter_stack_index;
						let counter_cell_index = self.stack_index_in_cell.len() + self.cell_init.len() - 1;
						self.emit(code![
							StackLoad(0), // Copy of output
							Constant(0x46000000), ExpandStereo, Mul, Round, // 0 when small
							SplitRL, Or, // 0 when both channels small
							Constant(0), Eq, // True when small
							StackLoad(counter_offset as u16), And, // Preserve counter when small
							Constant(0x3F800000), Add, // Increment counter
							StackLoad(0), CellStore(counter_cell_index as u16), // Update counter
							Constant(0x46000000), Less, // Has counter reached threshold?
							Kill // Kill when counter reaches threshold
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

	fn init_proc_id(&mut self, program: &Program<'ast>) {
		// Assign ID to all procedures in this order:
		// main, instruments, modules (except main), functions.
		self.proc_id.resize(program.procedures.len(), 0);
		self.proc_for_id.clear();

		let mut assign_ids = |pred: &dyn Fn(ProcedureKind, &str) -> bool, step: usize| {
			for (proc_index, proc) in program.procedures.iter().enumerate() {
				if pred(proc.kind, proc.name.text) {
					let id = self.proc_for_id.len() as u16;
					self.proc_id[proc_index] = id;
					for _ in 0..step {
						self.proc_for_id.push(proc_index);
					}
				}
			}
		};
		assign_ids(&|_, name| name == "main", 2);
		assign_ids(&|kind, _| kind == ProcedureKind::Instrument, 2);
		assign_ids(&|kind, name| kind == ProcedureKind::Module && name != "main", 2);
		assign_ids(&|kind, _| kind == ProcedureKind::Function, 1);
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
		for (kind, exp, width) in self.cell_init.clone() {
			self.generate(exp);
			match kind {
				StateKind::Cell => {
					self.emit(code![CellInit]);
				},
				StateKind::Delay => {
					self.emit(code![BufferAlloc(width), CellInit]);
				},
			}
		}
		self.generate_static_module_calls(&self.module_call.clone());
		Ok(())
	}

	fn generate_static_module_calls(&mut self, module_call: &Vec<ModuleCall<'ast>>) {
		for call in module_call {
			match call {
				&ModuleCall::Call { proc_index, generic_width, args } => {
					let (_, inputs, _) = &self.signatures[proc_index];
					for (input_type, arg) in inputs.clone().iter().zip(args) {
						if input_type.scope == Some(Scope::Static) {
							self.generate(arg);
						}
					}
					self.emit(code![Call(self.proc_id[proc_index], generic_width)]);
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
		let cell_stack_index_base = self.stack_height;
		for cell_index in 0 .. self.stack_index_in_cell.len() {
			if let Some(name) = self.name_in_cell.get(&cell_index) {
				self.stack_index.entry(name).or_insert(self.stack_height);
			}
			self.emit(code![CellRead]);
		}
		self.cell_stack_index = self.stack_height;
		for _ in 0 .. self.cell_init.len() {
			self.emit(code![CellRead]);
		}
		self.next_stack_index = self.stack_height;

		for statement in body {
			if statement_scope(statement) == Some(Scope::Dynamic) {
				self.generate_code_for_statement(statement)?;
			}
		}
		let mut cell_index = self.stack_index_in_cell.len();
		while let Some((kind, exp)) = self.update_queue.pop_front() {
			match kind {
				StateKind::Cell => {
					self.generate(exp);
					self.emit(code![CellStore(cell_index as u16)]);
				},
				StateKind::Delay => {
					let stack_index = cell_stack_index_base + cell_index;
					let offset = self.stack_height - stack_index - 1;
					self.emit(code![StackLoad(offset as u16)]);
					self.generate(exp);
					self.emit(code![BufferStoreAndStep, CellStore(cell_index as u16)]);
				}
			}
			cell_index += 1;
		}
		Ok(())
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
		self.cell_init.clear();
		self.module_call.clear();
		debug_assert!(self.update_queue.is_empty());

		for Statement::Assign { node, .. } in body {
			self.add_stack_indices(node, Some(Scope::Static), false, false);
		}

		for Statement::Assign { node, exp } in body {
			if node.items.iter().any(|item| item.item_type.scope == Some(Scope::Dynamic)) {
				self.find_cells(exp);
			}
		}
		while let Some((_kind, exp)) = self.update_queue.pop_front() {
			self.find_cells(exp);
		}

		self.compiler.check_errors()
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
			Call { name, args, .. } => {
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
										self.cell_init.push((StateKind::Cell, &args[0], width));
										self.update_queue.push_back((StateKind::Cell, &args[1]));
									},
									"delay" => {
										self.cell_init.push((StateKind::Delay, &args[0], width));
										self.update_queue.push_back((StateKind::Delay, &args[1]));
									},
									"dyndelay" => {
										self.cell_init.push((StateKind::Delay, &args[0], width));
										self.find_cells(&args[1]);
										self.update_queue.push_back((StateKind::Delay, &args[2]));
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
							(Instrument, Declaration { proc_index, .. }) => {
								self.instrument_order.push((self.proc_id[*proc_index] as usize - 2) / 2);
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
				let offset = match self.stack_index.get(name.text) {
					Some(stack_index) => self.stack_height - stack_index - 1,
					None => {
						self.unsupported(exp, "accessing a variable above its definition (except in cell or delay)");
						0
					},
				};
				self.emit(code![StackLoad(offset as u16)]);
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
										let offset = self.stack_height - self.cell_stack_index - 1;
										self.cell_stack_index += 1;
										self.emit(code![StackLoad(offset as u16)]);
										self.update_queue.push_back((StateKind::Cell, &args[1]));
									},
									"delay" => {
										let offset = self.stack_height - self.cell_stack_index - 1;
										self.cell_stack_index += 1;
										self.emit(code![StackLoad(offset as u16), BufferLoad]);
										self.update_queue.push_back((StateKind::Delay, &args[1]));
									},
									"dyndelay" => {
										let offset = self.stack_height - self.cell_stack_index - 1;
										self.cell_stack_index += 1;
										self.emit(code![StackLoad(offset as u16)]);
										self.generate(&args[1]);
										self.emit(code![BufferLoadWithOffset]);
										self.update_queue.push_back((StateKind::Delay, &args[2]));
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
								let proc_id = self.proc_id[*proc_index] + 1;
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
								let proc_id = self.proc_id[*proc_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp))]);
							},
							(Instrument, BuiltIn { .. }) => panic!("Built-in instrument"),
							(Instrument, Declaration { proc_index }) => {
								let (_, inputs, outputs) = &self.signatures[*proc_index];
								let (in_count, out_count) = (inputs.len() + 1, outputs.len());
								for arg in args {
									self.generate(arg);
								}
								self.emit(code![Constant(0), ExpandStereo]);
								self.emit(code![CallInstrument]);
								let stack_adjust: Vec<usize> = (in_count - out_count .. in_count).collect();
								self.adjust_stack(&stack_adjust[..], in_count);

								self.instrument_id += 2;
								self.proc_id[*proc_index] = self.instrument_id as u16;
								self.proc_for_id[self.instrument_id + 0] = *proc_index;
								self.proc_for_id[self.instrument_id + 1] = *proc_index;
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
