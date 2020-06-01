
use std::collections::{HashMap, VecDeque};
use std::mem::replace;

use crate::ast::*;
use crate::builtin::*;
use crate::bytecodes::*;
use crate::compiler::{CompileError, Compiler, Location};
use crate::names::*;

pub fn generate_code<'ast, 'input, 'comp>(
		program: &'ast Program<'ast>,
		names: &'ast Names<'ast>,
		signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,
		compiler: &mut Compiler<'input>) -> Result<Vec<Bytecode>, CompileError> {
	let mut cg = CodeGenerator::new(names, compiler, signatures);
	cg.generate_code_for_program(program)?;
	Ok(replace(&mut cg.bc, vec![]))
}

fn statement_scope<'ast>(statement: &Statement<'ast>) -> Option<Scope> {
	let Statement::Assign { node, .. } = statement;
	node.items.first().and_then(|item| item.item_type.scope)
}


#[derive(Clone)]
enum StateKind { Cell, Delay }

struct CodeGenerator<'ast, 'input, 'comp> {
	names: &'ast Names<'ast>,
	compiler: &'comp mut Compiler<'input>,
	signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>,

	/// Lowest id (of two for modules and instruments) for procedure
	proc_id: Vec<u16>,
	// Procedure index for id
	proc_for_id: Vec<usize>,
	// The id of last called instrument
	instrument_id: usize,

	bc: Vec<Bytecode>,
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
	// Static expression to initialize explicit cell
	cell_init: Vec<(StateKind, &'ast Expression<'ast>)>,
	// Module calls in execution order
	module_call: Vec<(usize, &'ast Vec<Expression<'ast>>)>,
	// Instruments in execution order
	instrument_order: Vec<usize>,
	// Queue for dynamic cell update expressions
	update_queue: VecDeque<(StateKind, &'ast Expression<'ast>)>,
}

impl<'ast, 'input, 'comp> CodeGenerator<'ast, 'input, 'comp> {
	pub fn new(
			names: &'ast Names<'ast>,
			compiler: &'comp mut Compiler<'input>,
			signatures: Vec<(ProcedureKind, Vec<Type>, Vec<Type>)>)
			-> CodeGenerator<'ast, 'input, 'comp> {
		CodeGenerator {
			names,
			compiler,
			signatures,

			proc_id: vec![],
			proc_for_id: vec![],
			instrument_id: 0,

			bc: vec![],
			stack_height: 0,

			next_stack_index: 0,
			stack_index: HashMap::new(),
			stack_index_in_cell: vec![],
			name_in_cell: HashMap::new(),
			cell_stack_index: 0,
			cell_init: vec![],
			module_call: vec![],
			instrument_order: vec![],
			update_queue: VecDeque::new(),
		}
	}

	fn unsupported(&mut self, loc: &dyn Location, what: &str) {
		self.compiler.report_error(loc, format!("Not supported yet: {}.", what));
	}

	fn emit(&mut self, bc: &[Bytecode]) {
		for &code in bc {
			let (popped, pushed) = match code {
				Bytecode::Proc => {
					// Assume stack height has been set
					(0, 0)
				},
				Bytecode::Call(id) => {
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
							(inputs.len(), inputs.len())
						},
					}
				},
				_ => code.stack_change(),
			};
			//println!("{:4}  {:2}  {:?}", self.bc.len(), self.stack_height, code);
			self.bc.push(code);
			if popped > self.stack_height {
				panic!(format!("Stack underflow: {:?}", self.bc));
			}
			self.stack_height -= popped;
			self.stack_height += pushed;
		}
	}

	pub fn generate_code_for_program(&mut self, program: &'ast Program<'ast>) -> Result<(), CompileError> {
		self.init_proc_id(program);
		for id in 0..self.proc_for_id.len() {
			let proc_index = self.proc_for_id[id];
			let Procedure { kind, inputs, outputs, body, .. } = &program.procedures[proc_index];
			self.emit(bc![Proc]);
			match kind {
				ProcedureKind::Module => {
					let is_static = (id & 1) == 0;
					if is_static {
						self.initialize_stack(inputs, Some(Scope::Static));
						self.find_cells_in_body(body)?;
						self.mark_implicit_cells_from_outputs(outputs);
						self.initialize_stack(inputs, Some(Scope::Static));
						self.generate_static_body(body)?;
						self.adjust_stack(&[], self.stack_height);
					} else {
						self.initialize_stack(inputs, Some(Scope::Dynamic));
						self.generate_dynamic_body(body)?;
						let stack_adjust = self.stack_adjust_from_outputs(outputs);
						self.adjust_stack(&stack_adjust[..], self.stack_height);
					}
				},
				ProcedureKind::Function => {
					self.initialize_stack(inputs, None);
					for statement in body {
						self.generate_code_for_statement(statement)?;
					}
					let stack_adjust = self.stack_adjust_from_outputs(outputs);
					self.adjust_stack(&stack_adjust[..], self.stack_height);
				},
				ProcedureKind::Instrument => {
					let is_static = (id & 1) == 0;
					if is_static {
						let static_inputs = Pattern {
							items: inputs.items.iter().map(|item| {
								if item.item_type.scope == Some(Scope::Static) {
									item.clone()
								} else {
									PatternItem {
										name: Id {
											text: "#dummy#", before: item.name.pos_before()
										},
										item_type: item.item_type,
									}
								}
							}).collect(),
							..*inputs
						};
						self.initialize_stack(&static_inputs, None);
						self.find_cells_in_body(body)?;
						self.mark_implicit_cells_from_outputs(outputs);
						self.initialize_stack(inputs, None);
						self.generate_static_body(body)?;
						self.adjust_stack(&[], self.stack_height - inputs.items.len());
					} else {
						self.initialize_stack(inputs, None);
						self.generate_dynamic_body(body)?;
						let stack_adjust: Vec<usize> = (0..(inputs.items.len() - outputs.items.len()))
							.chain(self.stack_adjust_from_outputs(outputs)).collect();
						self.adjust_stack(&stack_adjust[..], self.stack_height);
					}
				},
			}
		}
		self.emit(bc![Proc]);

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
			self.emit(bc![StackLoad(offset as u16), CellInit]);
			if let Some((name, _)) = self.stack_index.iter().find(|(_, i)| *i == stack_index) {
				self.name_in_cell.insert(cell_index, name);
			}
		}
		for (kind, exp) in self.cell_init.clone() {
			self.generate(exp);
			match kind {
				StateKind::Cell => {
					self.emit(bc![CellInit]);
				},
				StateKind::Delay => {
					self.emit(bc![BufferAlloc, Constant(0), MergeLR, CellInit]);
				},
			}
		}
		for (proc_index, args) in self.module_call.clone() {
			let (_, inputs, _) = &self.signatures[proc_index];
			for (input_type, arg) in inputs.clone().iter().zip(args) {
				if input_type.scope == Some(Scope::Static) {
					self.generate(arg);
				}
			}
			self.emit(bc![Call(self.proc_id[proc_index])]);
		}
		Ok(())
	}

	fn generate_dynamic_body(&mut self, body: &'ast Vec<Statement<'ast>>) -> Result<(), CompileError> {
		let cell_stack_index_base = self.stack_height;
		for cell_index in 0 .. self.stack_index_in_cell.len() {
			if let Some(name) = self.name_in_cell.get(&cell_index) {
				self.stack_index.entry(name).or_insert(self.stack_height);
			}
			self.emit(bc![CellRead]);
		}
		self.cell_stack_index = self.stack_height;
		for _ in 0 .. self.cell_init.len() {
			self.emit(bc![CellRead]);
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
					self.emit(bc![CellStore(cell_index as u16)]);
				},
				StateKind::Delay => {
					let stack_index = cell_stack_index_base + cell_index;
					let offset = self.stack_height - stack_index - 1;
					self.emit(bc![StackLoad(offset as u16), SplitLR]);
					self.generate(exp);
					self.emit(bc![
						BufferStore, Pop,
						StackLoad(offset as u16), SplitLR, Constant(1f32.to_bits()), Add, MergeLR,
						CellStore(cell_index as u16)
					]);
				}
			}
			cell_index += 1;
		}
		Ok(())
	}

	fn generate_code_for_statement(&mut self, statement: &'ast Statement<'ast>) -> Result<(), CompileError> {
		let Statement::Assign { node, exp } = statement;
		self.generate(exp);
		self.add_stack_indices(node, None, false);
		self.compiler.check_errors()
	}

	fn initialize_stack(&mut self, inputs: &Pattern<'ast>, scope: Option<Scope>) {
		self.stack_index.clear();
		self.next_stack_index = 0;
		self.stack_height = 0;
		self.add_stack_indices(inputs, scope, true);
	}

	fn add_stack_indices(&mut self, pattern: &Pattern<'ast>, scope: Option<Scope>, adjust_stack: bool) {
		for item in &pattern.items {
			if scope.is_none() || item.item_type.scope == scope {
				self.stack_index.insert(item.name.text, self.next_stack_index);
				self.next_stack_index += 1;
				if adjust_stack {
					self.stack_height += 1;
				}
			}
		}
	}

	fn find_cells_in_body(&mut self, body: &'ast Vec<Statement<'ast>>) -> Result<(), CompileError> {
		self.stack_index_in_cell.clear();
		self.cell_init.clear();
		self.module_call.clear();
		debug_assert!(self.update_queue.is_empty());

		for Statement::Assign { node, .. } in body {
			self.add_stack_indices(node, Some(Scope::Static), false);
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
								match name.text {
									"cell" => {
										self.cell_init.push((StateKind::Cell, &args[0]));
										self.update_queue.push_back((StateKind::Cell, &args[1]));
									},
									"delay" => {
										self.cell_init.push((StateKind::Delay, &args[0]));
										self.update_queue.push_back((StateKind::Delay, &args[1]));
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
								self.module_call.push((*proc_index, args));
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
								self.instrument_order.push(*proc_index);
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
			Expand { exp } => {
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
					self.emit(bc![Pop]);
				}
			},
			1 => {
				for _ in 0..(height - results[0] - 1) {
					self.emit(bc![Pop]);
				}
				for _ in 0..results[0] {
					self.emit(bc![PopNext]);
				}
			},
			_ => {
				for i in 0..results.len() {
					if results[i] != i {
						let offset = height - results[i] - 1;
						self.emit(bc![StackLoad(offset as u16)]);
						height += 1;
					}
				}
				for i in (0..results.len()).rev() {
					if results[i] != i {
						height -= 1;
						let offset = height - i - 1;
						self.emit(bc![StackStore(offset as u16)]);
					}
				}
				for _ in 0..(height - results.len()) {
					self.emit(bc![Pop]);
				}
			},
		}
	}

	fn generate(&mut self, exp: &'ast Expression<'ast>) {
		use Expression::*;
		match exp {
			Number { value, .. } => self.emit(bc![Constant((*value as f32).to_bits())]),
			Bool { value, .. } => if *value {
				self.emit(bc![Constant(0), Constant(0), Compare(Eq)]);
			} else {
				self.emit(bc![Constant(0)]);
			},
			Variable { name } => {
				let offset = match self.stack_index.get(name.text) {
					Some(stack_index) => self.stack_height - stack_index - 1,
					None => {
						self.unsupported(exp, "accessing a variable above its definition (except in cell or delay)");
						0
					},
				};
				self.emit(bc![StackLoad(offset as u16)]);
			},
			UnOp { op, exp } => {
				self.generate(exp);
				self.emit(op.bytecodes());
			},
			BinOp { left, op, right } => {
				self.generate(right);
				self.generate(left);
				self.emit(op.bytecodes());
			},
			Conditional { condition, then, otherwise } => {
				self.generate(condition);
				self.generate(then);
				self.emit(bc![StackLoad(1), And]);
				self.generate(otherwise);
				self.emit(bc![StackLoad(2), AndNot, Or, PopNext]);
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
										self.emit(bc![StackLoad(offset as u16)]);
										self.update_queue.push_back((StateKind::Cell, &args[1]));
									},
									"delay" => {
										let offset = self.stack_height - self.cell_stack_index - 1;
										self.cell_stack_index += 1;
										self.emit(bc![StackLoad(offset as u16), SplitLR, BufferLoad]);
										self.update_queue.push_back((StateKind::Delay, &args[1]));
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
								self.emit(bc![Call(proc_id)]);
							},
							(Function, BuiltIn { bc, .. }) => {
								for arg in args {
									self.generate(arg);
								}
								self.emit(bc);
							},
							(Function, Declaration { proc_index }) => {
								for arg in args {
									self.generate(arg);
								}
								let proc_id = self.proc_id[*proc_index];
								self.emit(bc![Call(proc_id)]);
							},
							(Instrument, BuiltIn { .. }) => panic!("Built-in instrument"),
							(Instrument, Declaration { proc_index }) => {
								let (_, inputs, outputs) = &self.signatures[*proc_index];
								let (in_count, out_count) = (inputs.len(), outputs.len());
								for arg in args {
									self.generate(arg);
								}
								self.emit(bc![CallInstrument]);
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
				self.emit(bc![MergeLR]);
			},
			Property { exp, name } => {
				self.generate(exp);
				match name.text {
					"left" => {},
					"right" => {
						self.emit(bc![SplitRL, PopNext]);
					},
					"length" => {
						self.emit(bc![BufferLength]);
					},
					_ => panic!("Unknown property"),
				}
			},
			TupleIndex { .. } => {
				self.unsupported(exp, "tuple indexing");
			},
			BufferIndex { exp, index, .. } => {
				self.generate(exp);
				self.generate(index);
				self.emit(bc![BufferLoad]);
			},
			Expand { exp } => {
				self.generate(exp);
				self.emit(bc![Expand]);
			},
		}
	}
}
