
use std::collections::{HashMap, HashSet};
use std::mem::{replace, take};

use ir::{code, Instruction};

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::*;
use crate::names::*;
use crate::type_inference::FullSignature;

pub fn generate_code<'ast, 'comp, 'names>(
		program: &'ast Program,
		names: &'names Names,
		signatures: Vec<FullSignature>,
		stored_widths: HashMap<*const Expression, Width>,
		callees: Vec<Vec<usize>>,
		precompiled_callees: Vec<Vec<*const PrecompiledProcedure>>,
		compiler: &mut Compiler)
-> Result<(Vec<ir::Procedure>, usize, usize, Vec<usize>), CompileError> {
	let mut cg = CodeGenerator::new(names, compiler, signatures, stored_widths, callees, precompiled_callees);
	let main_index = match names.lookup_procedure(&"main".to_string()).unwrap().definition {
		ProcedureDefinition::Declaration { proc_index } => proc_index,
		_ => panic!("No main"),
	};
	cg.generate_code_for_program(program, main_index)?;
	let main_static_proc_id = cg.static_proc_id[main_index] as usize;
	let main_dynamic_proc_id = cg.dynamic_proc_id[main_index] as usize;
	let track_order = cg.compute_track_order(main_index);
	Ok((take(&mut cg.procedures), main_static_proc_id, main_dynamic_proc_id, track_order))
}

fn statement_scope(statement: &Statement) -> Option<Scope> {
	let Statement::Assign { node, .. } = statement;
	node.items.first().and_then(|item| item.item_type.scope)
}


#[derive(Clone, Copy)]
enum StateKind { Cell, Delay }

#[derive(Clone)]
enum ModuleCall<'ast> {
	Init {
		kind: StateKind,
		value: &'ast Expression,
		width: Width,
	},
	Call {
		inputs: Vec<Type>,
		static_proc_id: u16,
		generic_width: Option<Width>,
		args: &'ast Vec<Expression>,
	},
	For {
		name: &'ast Id,
		count: &'ast Expression,
		nested_calls: Vec<ModuleCall<'ast>>,
	},
}

trait ToIR {
	type IR;
	fn to_ir(self) -> Self::IR;
}

impl ToIR for Width {
	type IR = ir::Width;
	fn to_ir(self) -> ir::Width {
		match self {
			Width::Mono => ir::Width::Mono,
			Width::Stereo => ir::Width::Stereo,
			Width::Generic => ir::Width::Generic,
		}
	}
}

impl ToIR for Option<Width> {
	type IR = Option<ir::Width>;
	fn to_ir(self) -> Option<ir::Width> {
		self.map(|w| w.to_ir())
	}
}

impl ToIR for ValueType {
	type IR = ir::ValueType;
	fn to_ir(self) -> ir::ValueType {
		match self {
			ValueType::Number => ir::ValueType::Number,
			ValueType::Bool => ir::ValueType::Number,
			ValueType::Buffer => ir::ValueType::Buffer,
			ValueType::Typeless => panic!("Typeless parameter in signature"),
		}
	}
}

impl ToIR for Type {
	type IR = ir::Type;
	fn to_ir(self) -> ir::Type {
		ir::Type {
			width: self.width.unwrap().to_ir(),
			value_type: self.value_type.unwrap().to_ir(),
		}
	}
}

struct CodeGenerator<'ast, 'comp, 'names> {
	names: &'names Names,
	compiler: &'comp mut Compiler,
	signatures: Vec<FullSignature>,
	stored_widths: HashMap<*const Expression, Width>,
	callees: Vec<Vec<usize>>,
	precompiled_callees: Vec<Vec<*const PrecompiledProcedure>>,

	/// Is the procedure reachable from main?
	live: Vec<bool>,
	/// Is the precompiled procedure reachable from main?
	live_precompiled: HashSet<*const PrecompiledProcedure>,
	/// Procedure ID for functions
	function_proc_id: Vec<u16>,
	/// Static procedure ID for modules and instruments
	static_proc_id: Vec<u16>,
	/// Dynamic procedure ID for modules and instruments
	dynamic_proc_id: Vec<u16>,
	/// Procedure IDs for precompiled procedures
	precompiled_proc_ids: HashMap<*const PrecompiledProcedure, Vec<u16>>,
	/// Procedure and scope for ID
	proc_for_id: Vec<(ProcedureRef, Option<Scope>)>,
	/// The current procedure index
	current_proc_index: usize,
	/// The current procedure kind
	current_kind: ProcedureKind,
	/// The current procedure scope
	current_scope: Option<Scope>,

	procedures: Vec<ir::Procedure>,
	code: Vec<Instruction>,
	stack_height: usize,

	/// Next stack index
	next_stack_index: usize,
	/// Stack index of variable
	stack_index: HashMap<String, usize>,
	/// Stack index of static variable in implicit cell
	stack_index_in_cell: Vec<usize>,
	// Variable names of implicit cells
	name_in_cell: HashMap<usize, String>,
	// Nesting depth of repetitions
	repetition_depth: usize,
	// Module calls in execution order
	module_call: Vec<ModuleCall<'ast>>,
	// Tracks in execution order
	track_order: Vec<Vec<TrackOrderNode>>,
	// Queue for dynamic cell update expressions
	update_stack: Vec<(StateKind, &'ast Expression)>,
}

#[derive(Clone, Debug)]
enum TrackOrderNode {
	Instrument { channel: MidiChannelArg },
	Module { proc_index: usize, args: Vec<MidiChannelArg> },
}

#[derive(Clone, Debug)]
enum MidiChannelArg {
	Value { channel: usize },
	Input { index: usize },
}

impl<'ast, 'comp, 'names> CodeGenerator<'ast, 'comp, 'names> {
	pub fn new(
			names: &'names Names,
			compiler: &'comp mut Compiler,
			signatures: Vec<FullSignature>,
			stored_widths: HashMap<*const Expression, Width>,
			callees: Vec<Vec<usize>>,
			precompiled_callees: Vec<Vec<*const PrecompiledProcedure>>)
			-> CodeGenerator<'ast, 'comp, 'names> {
		let proc_count = callees.len();
		CodeGenerator {
			names,
			compiler,
			signatures,
			stored_widths,
			callees,
			precompiled_callees,

			live: vec![false; proc_count],
			live_precompiled: HashSet::new(),
			function_proc_id: vec![0; proc_count],
			static_proc_id: vec![0; proc_count],
			dynamic_proc_id: vec![0; proc_count],
			precompiled_proc_ids: HashMap::new(),
			proc_for_id: vec![],
			current_proc_index: 0,
			current_kind: ProcedureKind::Module,
			current_scope: None,

			procedures: vec![],
			code: vec![],
			stack_height: 0,

			next_stack_index: 0,
			stack_index: HashMap::new(),
			stack_index_in_cell: vec![],
			name_in_cell: HashMap::new(),
			repetition_depth: 0,
			module_call: vec![],
			track_order: vec![vec![]; proc_count],
			update_stack: vec![],
		}
	}

	fn retrieve_width(&self, exp: &Expression) -> Option<Width> {
		self.stored_widths.get(&(exp as *const Expression)).copied()
	}

	fn unsupported(&mut self, loc: &dyn Location, what: &str) {
		self.compiler.report_error(loc, format!("Not supported yet: {}.", what));
	}

	fn emit(&mut self, code: &[Instruction]) {
		for &inst in code {
			let (popped, pushed) = match inst {
				Instruction::Call(id, ..) => {
					let (ref proc, scope) = self.proc_for_id[id as usize];
					let (inputs, outputs) = match &proc.definition {
						ProcedureDefinition::Declaration { proc_index } => {
							let FullSignature { inputs, outputs, .. } = &self.signatures[*proc_index];
							(&inputs[..], &outputs[..])
						},
						ProcedureDefinition::Precompiled { proc } => {
							(proc.inputs(), proc.outputs())
						},
						ProcedureDefinition::BuiltIn { .. } => {
							panic!("Call of built-in procedure");
						},
					};
					match proc.kind {
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

	fn compute_track_order(&mut self, main_index: usize) -> Vec<usize> {
		let mut track_order = vec![];
		self.compute_track_order_inner(main_index, &vec![], &mut track_order);
		track_order
	}

	fn resolve_midi_channel_arg(&self, channel: &MidiChannelArg, inputs: &Vec<MidiChannelArg>) -> usize {
		match channel {
			MidiChannelArg::Value { channel } => channel - 1,
			MidiChannelArg::Input { index } => self.resolve_midi_channel_arg(&inputs[*index], inputs),
		}
	}

	fn compute_track_order_inner(&mut self, proc_index: usize, inputs: &Vec<MidiChannelArg>, track_order: &mut Vec<usize>) {
		for node in self.track_order[proc_index].clone() {
			match node {
				TrackOrderNode::Instrument { channel } => {
					track_order.push(self.resolve_midi_channel_arg(&channel, inputs));
				},
				TrackOrderNode::Module { proc_index, args  } => {
					self.compute_track_order_inner(proc_index, &args, track_order);
				},
			}
		}
	}

	pub fn generate_code_for_program(&mut self, program: &'ast Program, main_index: usize) -> Result<(), CompileError> {
		// Compute liveness
		self.propagate_liveness(main_index);
		let autokill_key = self.lookup_precompiled("$autokill");
		self.live_precompiled.insert(autokill_key);

		self.init_proc_id(program);
		for id in 0..self.proc_for_id.len() {
			let (ref proc, scope) = self.proc_for_id[id];
			let proc = match proc.definition {
				ProcedureDefinition::Declaration { proc_index } => {
					self.generate_code_for_procedure(program, proc_index, scope)?
				},
				ProcedureDefinition::Precompiled { proc } => {
					let (kind, index) = match scope {
						None => (ir::ProcedureKind::Function, 0),
						Some(Scope::Static) => (ir::ProcedureKind::Module { scope: ir::Scope::Static }, 0),
						Some(Scope::Dynamic) => (ir::ProcedureKind::Module { scope: ir::Scope::Dynamic }, 1),
					};
					ir::Procedure {
						name: proc.name().to_string(),
						kind,
						inputs: self.make_type_list(proc.inputs().iter().copied(), scope),
						outputs: self.make_type_list(proc.outputs().iter().copied(), scope),
						code: proc.instructions()[index].to_vec()
					}
				},
				ProcedureDefinition::BuiltIn { .. } => {
					panic!("Code generation for built-in procedure");
				},
			};
			self.procedures.push(proc);
		}
		self.compiler.check_errors()
	}

	fn generate_code_for_procedure(&mut self,
		program: &'ast Program,
		proc_index: usize,
		scope: Option<Scope>,
	) -> Result<ir::Procedure, CompileError> {
		let Procedure { kind, name, inputs, outputs, body, .. } = &program.procedures[proc_index];
		self.current_proc_index = proc_index;
		self.current_kind = *kind;
		self.current_scope = scope;
		let proc_kind;
		let proc_inputs;
		let proc_outputs;
		match kind {
			ProcedureKind::Module => {
				if scope == Some(Scope::Static) {
					proc_kind = ir::ProcedureKind::Module { scope: ir::Scope::Static };
					proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Static));
					proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Static));
					self.initialize_stack(inputs, Some(Scope::Static), false);
					self.find_cells_in_body(body)?;
					self.mark_implicit_cells_from_outputs(outputs);
					self.initialize_stack(inputs, Some(Scope::Static), false);
					self.generate_static_body(body)?;
					self.adjust_stack(&[], self.stack_height);
				} else {
					proc_kind = ir::ProcedureKind::Module { scope: ir::Scope::Dynamic };
					proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Dynamic));
					proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Dynamic));
					self.initialize_stack(inputs, Some(Scope::Dynamic), false);
					self.generate_dynamic_body(body)?;
					let stack_adjust = self.stack_adjust_from_outputs(outputs);
					self.adjust_stack(&stack_adjust[..], self.stack_height);
				}
			},
			ProcedureKind::Function => {
				proc_kind = ir::ProcedureKind::Function;
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
						text: "#acc#".to_string(), before: outputs.before,
					},
					item_type: type_spec!(dynamic stereo number),
				});
				let autokill_key = self.lookup_precompiled("$autokill");
				if scope == Some(Scope::Static) {
					proc_kind = ir::ProcedureKind::Instrument { scope: ir::Scope::Static };
					self.initialize_stack(&real_inputs, Some(Scope::Static), true);
					self.find_cells_in_body(body)?;
					self.mark_implicit_cells_from_outputs(outputs);
					self.initialize_stack(&real_inputs, Some(Scope::Static), true);
					self.generate_static_body(body)?;
					// Init autokill
					self.emit(code![Call(self.precompiled_proc_ids[&autokill_key][0], None)]);
					// Leave only the inputs (including the accumulator) on the stack.
					self.adjust_stack(&[], self.stack_height - real_inputs.items.len());
				} else {
					proc_kind = ir::ProcedureKind::Instrument { scope: ir::Scope::Dynamic };
					self.initialize_stack(&real_inputs, Some(Scope::Dynamic), true);
					self.generate_dynamic_body(body)?;
					// Leave the inputs (including the accumulator) and the output on the stack.
					let mut stack_adjust: Vec<usize> = (0..(inputs.items.len() + 1)).collect();
					stack_adjust.push(self.stack_index[&outputs.items[0].name.text]);
					self.adjust_stack(&stack_adjust, self.stack_height);
					// Run autokill code
					self.emit(code![Call(self.precompiled_proc_ids[&autokill_key][1], None)]);
				}
				proc_inputs = self.make_proc_type_list(inputs, None);
				proc_outputs = self.make_proc_type_list(inputs, None);
			},
		}

		Ok(ir::Procedure {
			name: name.text.clone(),
			kind: proc_kind,
			inputs: proc_inputs,
			outputs: proc_outputs,
			code: take(&mut self.code),
		})
	}

	fn propagate_liveness(&mut self, proc_index: usize) {
		if !self.live[proc_index] {
			self.live[proc_index] = true;
			for &callee in &self.callees[proc_index].clone() {
				self.propagate_liveness(callee);
			}
			for precompiled_callee in &self.precompiled_callees[proc_index] {
				self.live_precompiled.insert(*precompiled_callee);
			}
		}
	}

	fn assign_ids(&mut self, program: &Program, pred: &dyn Fn(ProcedureKind, &str) -> bool) {
		for (proc_index, proc) in program.procedures.iter().enumerate() {
			if self.live[proc_index] && pred(proc.kind, proc.name.text.as_str()) {
				let mut push_id = |proc_id: &mut Vec<u16>, scope: Option<Scope>| {
					proc_id[proc_index] = self.proc_for_id.len() as u16;
					let proc = ProcedureRef {
						context: proc.context,
						kind: proc.kind,
						definition: ProcedureDefinition::Declaration { proc_index }
					};
					self.proc_for_id.push((proc, scope));
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
	}

	fn assign_precompiled_ids(&mut self,
		procs: &'static [PrecompiledProcedure],
		kind: ProcedureKind,
		scopes: &[Option<Scope>]
	) {
		for proc in procs {
			if self.live_precompiled.contains(&(proc as *const PrecompiledProcedure)) {
				for &scope in scopes {
					self.precompiled_proc_ids.entry(proc).or_default().push(self.proc_for_id.len() as u16);
					let proc = ProcedureRef {
						context: proc.context(),
						kind: kind,
						definition: ProcedureDefinition::Precompiled { proc }
					};
					self.proc_for_id.push((proc, scope));
				};
			}
		}
	}

	fn init_proc_id(&mut self, program: &Program) {
		// Assign ID to all procedures in this order:
		// main, instruments, modules (except main), functions.
		self.assign_ids(program, &|_, name| name == "main");
		self.assign_ids(program, &|kind, _| kind == ProcedureKind::Instrument);
		self.assign_precompiled_ids(PRECOMPILED_MODULES, ProcedureKind::Module, &[Some(Scope::Static), Some(Scope::Dynamic)]);
		self.assign_ids(program, &|kind, name| kind == ProcedureKind::Module && name != "main");
		self.assign_precompiled_ids(PRECOMPILED_FUNCTIONS, ProcedureKind::Function, &[None]);
		self.assign_ids(program, &|kind, _| kind == ProcedureKind::Function);
	}

	fn lookup_precompiled(&mut self, name: &str) -> *const PrecompiledProcedure {
		let proc = self.names.lookup_procedure(&name.to_string());
		let Some(ProcedureRef { definition: ProcedureDefinition::Precompiled { proc }, .. }) = proc else {
			panic!("Precompiled procedure not found: {}", name);
		};
		*proc as *const PrecompiledProcedure
	}

	fn generate_static_body(&mut self, body: &'ast Vec<Statement>) -> Result<(), CompileError> {
		for statement in body {
			if statement_scope(statement) == Some(Scope::Static) {
				self.generate_code_for_statement(statement)?;
			}
		}
		for (cell_index, stack_index) in self.stack_index_in_cell.clone().iter().enumerate() {
			let offset = self.stack_height - stack_index - 1;
			self.emit(code![StackLoad(offset as u16), CellInit]);
			if let Some((name, _)) = self.stack_index.iter().find(|(_, i)| *i == stack_index) {
				self.name_in_cell.insert(cell_index, name.clone());
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
							self.emit(code![BufferAlloc(width.to_ir()), CellInit]);
						},
					}
				},
				&ModuleCall::Call { ref inputs, static_proc_id, generic_width, args } => {
					for (input_type, arg) in inputs.clone().iter().zip(args) {
						if input_type.scope == Some(Scope::Static) {
							self.generate(arg);
						}
					}
					self.emit(code![Call(static_proc_id, generic_width.to_ir())]);
				},
				ModuleCall::For { name, count, nested_calls } => {
					self.generate(count);
					self.emit(code![RepeatInit]);
					let counter_stack_index = self.stack_height - 1;
					self.stack_index.insert(name.text.clone(), counter_stack_index);
					self.generate_static_module_calls(nested_calls);
					self.stack_index.remove(&name.text);
					self.emit(code![RepeatEnd]);
				},
			}
		}
	}

	fn generate_dynamic_body(&mut self, body: &'ast Vec<Statement>) -> Result<(), CompileError> {
		for cell_index in 0 .. self.stack_index_in_cell.len() {
			if let Some(name) = self.name_in_cell.get(&cell_index) {
				self.stack_index.entry(name.to_string()).or_insert(self.stack_height);
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

	fn generate_code_for_statement(&mut self, statement: &'ast Statement) -> Result<(), CompileError> {
		let Statement::Assign { node, exp } = statement;
		self.generate(exp);
		self.add_stack_indices(node, None, false, false);
		self.compiler.check_errors()
	}

	fn initialize_stack(&mut self, inputs: &Pattern, scope: Option<Scope>, all_scopes: bool) {
		self.stack_index.clear();
		self.next_stack_index = 0;
		self.stack_height = 0;
		self.add_stack_indices(inputs, scope, true, all_scopes);
	}

	fn add_stack_indices(&mut self, pattern: &Pattern, scope: Option<Scope>, adjust_stack: bool, all_scopes: bool) {
		for item in &pattern.items {
			if (scope.is_none() || item.item_type.scope == scope) && item.name.text != "_" {
				self.stack_index.insert(item.name.text.clone(), self.next_stack_index);
			}
			if scope.is_none() || item.item_type.scope == scope || all_scopes {
				self.next_stack_index += 1;
				if adjust_stack {
					self.stack_height += 1;
				}
			}
		}
	}

	fn make_proc_type_list(&self, types: &Pattern, scope: Option<Scope>) -> Vec<ir::Type> {
		self.make_type_list(types.items.iter().map(|item| item.item_type), scope)
	}

	fn make_type_list(&self, types: impl IntoIterator<Item = Type>, scope: Option<Scope>) -> Vec<ir::Type> {
		types.into_iter()
			.filter(|item_type| scope.is_none() || item_type.scope == scope)
			.map(|item_type| item_type.to_ir())
			.collect()
	}

	fn find_cells_in_body(&mut self, body: &'ast Vec<Statement>) -> Result<(), CompileError> {
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

	fn mark_implicit_cells_from_outputs(&mut self, outputs: &'ast Pattern) {
		for item in &outputs.items {
			self.mark_implicit_cell(&item.name);
		}
	}

	fn mark_implicit_cell(&mut self, name: &'ast Id) {
		if let Some(index) = self.stack_index.get(&name.text) {
			// Static variable
			if let None = self.stack_index_in_cell.iter().find(|&i| i == index) {
				self.stack_index_in_cell.push(*index);
			}
		}
	}

	fn find_cells(&mut self, exp: &'ast Expression) {
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
			Call { channels, name, args, .. } => {
				match self.names.lookup_procedure(&name.text) {
					Some(ProcedureRef { kind, definition, .. }) => {
						let current_proc_index = self.current_proc_index;
						let convert_midi_channel = |channel: &MidiChannel| -> MidiChannelArg {
							match channel {
								&MidiChannel::Value { channel } => MidiChannelArg::Value {
									channel
								},
								MidiChannel::Named { name } => MidiChannelArg::Input {
									index: self.names.lookup_midi_input(current_proc_index, &name.text).unwrap()
								},
							}
						};

						use ProcedureKind::*;
						use ProcedureDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								if self.repetition_depth > 0 {
									self.unsupported(exp, "Built-in module in repetition body");
								}
								let width = self.retrieve_width(exp).unwrap();
								match name.text.as_str() {
									"cell" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Cell, value: &args[1], width });
										self.update_stack.push((StateKind::Cell, &args[0]));
									},
									"delay" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[1], width });
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									"dyndelay" => {
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[2], width });
										self.find_cells(&args[1]);
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									_ => panic!("Unknown built-in module"),
								}
							},
							(Module, Precompiled { proc }) => {
								let inputs = proc.inputs();
								for (arg, input_type) in args.iter().zip(inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.find_cells(arg);
									}
								}
								let key = *proc as *const PrecompiledProcedure;
								self.module_call.push(ModuleCall::Call {
									inputs: inputs.to_vec(),
									static_proc_id: self.precompiled_proc_ids[&key][0],
									generic_width: self.retrieve_width(exp),
									args,
								});
							},
							(Module, Declaration { proc_index }) => {
								let proc_index = *proc_index;
								let FullSignature { context, inputs, .. } = &self.signatures[proc_index];
								let context = *context;
								let inputs = inputs.clone();
								for (arg, input_type) in args.iter().zip(&inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.find_cells(arg);
									}
								}
								self.module_call.push(ModuleCall::Call {
									inputs: inputs,
									static_proc_id: self.static_proc_id[proc_index],
									generic_width: self.retrieve_width(exp),
									args,
								});
								if context == Context::Global {
									let args = channels.iter().map(convert_midi_channel).collect();
									let node = TrackOrderNode::Module { proc_index, args };
									self.track_order[current_proc_index].push(node);
								}
							},
							(Function, _) => {
								for arg in args {
									self.find_cells(arg);
								}
							},
							(Instrument, Declaration { .. }) => {
								let channel = convert_midi_channel(&channels[0]);
								let node = TrackOrderNode::Instrument { channel };
								self.track_order[current_proc_index].push(node);
								for arg in args {
									self.find_cells(arg);
								}
							},
							(Instrument, _) => panic!("Built-in instrument"),
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
			BufferInit { buffer_type, .. } => {
				self.module_call.push(ModuleCall::Init {
					kind: StateKind::Cell,
					value: exp,
					width: buffer_type.width.unwrap(),
				});
			},
			Expand { exp, .. } => {
				self.find_cells(exp);
			}
		}
	}

	fn stack_adjust_from_outputs(&mut self, outputs: &'ast Pattern) -> Vec<usize> {
		let mut stack_adjust = vec![];
		for item in &outputs.items {
			let index = self.stack_index[&item.name.text];
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

	fn generate(&mut self, exp: &'ast Expression) {
		use Expression::*;
		match exp {
			Number { value, .. } => self.emit(code![Constant((*value as f32).to_bits())]),
			Bool { value, .. } => if *value {
				self.emit(code![Constant(0), Constant(0), Eq]);
			} else {
				self.emit(code![Constant(0)]);
			},
			Variable { name } => {
				match self.stack_index.get(&name.text) {
					Some(stack_index) => {
						let offset = self.stack_height - stack_index - 1;
						self.emit(code![StackLoad(offset as u16)]);
					},
					None => {
						match self.names.lookup_variable(self.current_proc_index, &name.text) {
							Some(VariableRef::Parameter { index }) => {
								self.emit(code![Parameter(*index as u16)]);
							},
							Some(VariableRef::Node { .. }) => {
								self.compiler.report_error(name, "Reference to a later variable is only allowed in a cell or delay.");
								// Dummy push to keep stack height consistent
								self.emit(code![Constant(0)]);
							},
							Some(VariableRef::For { variable_pos }) => {
								self.compiler.report_error(name, "An iteration variable can only be used inside its repetition.");
								self.compiler.report_context(variable_pos, format!("Iteration variable '{}' defined here.", name));
								// Dummy push to keep stack height consistent
								self.emit(code![Constant(0)]);
							},
							_ => unreachable!("Variable not found"),
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
				match self.names.lookup_procedure(&name.text) {
					Some(ProcedureRef { kind, definition, .. }) => {
						use ProcedureKind::*;
						use ProcedureDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								match name.text.as_str() {
									"cell" => {
										self.emit(code![CellPush]);
										self.update_stack.push((StateKind::Cell, &args[0]));
									},
									"delay" => {
										self.emit(code![CellPush, BufferLoad]);
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									"dyndelay" => {
										self.emit(code![CellPush]);
										self.generate(&args[1]);
										self.emit(code![BufferLoadWithOffset]);
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									_ => panic!("Unknown built-in module"),
								}
							},
							(Module, Precompiled { proc }) => {
								let inputs = proc.inputs();
								for (arg, input_type) in args.iter().zip(inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.generate(arg);
									}
								}
								let key = *proc as *const PrecompiledProcedure;
								let proc_id = self.precompiled_proc_ids[&key][1];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Module, Declaration { proc_index }) => {
								let FullSignature { inputs, .. } = &self.signatures[*proc_index];
								let inputs = inputs.clone();
								for (arg, input_type) in args.iter().zip(&inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.generate(arg);
									}
								}
								let proc_id = self.dynamic_proc_id[*proc_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Function, BuiltIn { code, .. }) => {
								for arg in args {
									self.generate(arg);
								}
								self.emit(code);
							},
							(Function, Precompiled { proc }) => {
								for arg in args {
									self.generate(arg);
								}
								let key = *proc as *const PrecompiledProcedure;
								let proc_id = self.precompiled_proc_ids[&key][0];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Function, Declaration { proc_index }) => {
								for arg in args {
									self.generate(arg);
								}
								let proc_id = self.function_proc_id[*proc_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Instrument, Declaration { proc_index }) => {
								let static_proc_id = self.static_proc_id[*proc_index];
								let dynamic_proc_id = self.dynamic_proc_id[*proc_index];

								let FullSignature { inputs, outputs, .. } = &self.signatures[*proc_index];
								let (in_count, out_count) = (inputs.len() + 1, outputs.len());
								for arg in args {
									self.generate(arg);
								}
								self.emit(code![Constant(0), ExpandStereo]);
								self.emit(code![PlayInstrument(static_proc_id, dynamic_proc_id)]);
								let stack_adjust: Vec<usize> = (in_count - out_count .. in_count).collect();
								self.adjust_stack(&stack_adjust[..], in_count);
							},
							(Instrument, _) => panic!("Built-in instrument"),
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
			TupleIndex { .. } => {
				self.unsupported(exp, "tuple indexing");
			},
			BufferIndex { exp, index, .. } => {
				self.generate(exp);
				self.generate(index);
				self.emit(code![BufferLoadIndexed]);
			},
			For { name, body, combinator, .. } => {
				let combinator = self.names.lookup_combinator(&combinator.text).unwrap();
				self.emit(code![Constant(combinator.neutral.to_bits())]); // accumulator
				self.expand(self.retrieve_width(exp).unwrap());
				self.emit(code![RepeatStart]);
				let counter_stack_index = self.stack_height - 1;
				self.stack_index.insert(name.text.clone(), counter_stack_index);
				self.generate(body);
				self.stack_index.remove(&name.text);
				self.emit(code![StackLoad(3)]); // accumulator
				self.emit(combinator.code);
				self.emit(code![StackStore(2)]); // accumulator
				self.emit(code![RepeatEnd]);
			},
			BufferInit { length, body, .. } => {
				match self.current_scope {
					Some(Scope::Static) => {
						let Expression::Call { ref name, ref args, .. } = **body else {
							panic!("Buffer initialization must be a call");
						};
						let proc_index = match self.names.lookup_procedure(&name.text) {
							Some(ProcedureRef { definition: ProcedureDefinition::Declaration { proc_index }, .. }) => proc_index,
							_ => panic!("Buffer initialization must be a call"),
						};

						let static_proc_id = self.static_proc_id[*proc_index];
						let dynamic_proc_id = self.dynamic_proc_id[*proc_index];

						let body_width = self.retrieve_width(body);
						let buffer_width = self.retrieve_width(exp);

						self.generate(length);
						for arg in args {
							self.generate(arg);
						}
						self.emit(code![
							StateEnter,
							Call(static_proc_id, body_width.to_ir()),
							StateLeave,
							BufferAlloc(buffer_width.unwrap_or(Width::Mono).to_ir()),
							BufferInitStart,
							StateEnter,
							Call(dynamic_proc_id, body_width.to_ir()),
							StateLeave
						]);
						if let Some(buffer_width) = buffer_width {
							self.expand(buffer_width);
						}
						self.emit(code![
							BufferInitEnd
						]);
					},
					Some(Scope::Dynamic) => {
						// Already evaluated and stored in a cell
						self.emit(code![CellRead]);
					},
					None => panic!("Buffer initialization in a function"),
				}
			},
			Expand { exp, width } => {
				self.generate(exp);
				self.expand(*width);
			},
		}
	}

	fn expand(&mut self, width: Width) {
		match width {
			Width::Mono => {},
			Width::Stereo => self.emit(code![ExpandStereo]),
			Width::Generic => self.emit(code![ExpandGeneric]),
		}
	}
}
