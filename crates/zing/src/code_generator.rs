
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
		precompiled_callees: Vec<Vec<*const PrecompiledMember>>,
		compiler: &mut Compiler)
-> Result<(Vec<ir::Procedure>, usize, usize, Vec<usize>), CompileError> {
	let mut cg = CodeGenerator::new(names, compiler, signatures, stored_widths, callees, precompiled_callees);
	let main_index = match names.lookup_member(&"main".to_string()).unwrap().definition {
		MemberDefinition::Declaration { member_index } => member_index,
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
	ImplicitCell {
		stack_index: usize,
		cell_type: ir::Type,
	},
	Init {
		kind: StateKind,
		value: &'ast Expression,
		cell_type: ir::Type,
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
	precompiled_callees: Vec<Vec<*const PrecompiledMember>>,

	/// Is the member reachable from main?
	live: Vec<bool>,
	/// Is the precompiled member reachable from main?
	live_precompiled: HashSet<*const PrecompiledMember>,
	/// Procedure ID for functions
	function_proc_id: Vec<u16>,
	/// Static procedure ID for modules and instruments
	static_proc_id: Vec<u16>,
	/// Dynamic procedure ID for modules and instruments
	dynamic_proc_id: Vec<u16>,
	/// Procedure IDs for precompiled members
	precompiled_proc_ids: HashMap<*const PrecompiledMember, Vec<u16>>,
	/// Member and scope for ID
	member_for_id: Vec<(MemberRef, Option<Scope>)>,
	/// The current member index
	current_member_index: usize,
	/// The current member kind
	current_kind: MemberKind,
	/// The current member scope
	current_scope: Option<Scope>,

	procedures: Vec<ir::Procedure>,
	code: Vec<Instruction>,
	stack_height: usize,

	/// Next stack index
	next_stack_index: usize,
	/// Stack index of variable
	stack_index: HashMap<String, usize>,
	/// Static-phase stack position of every static-scope name (input or assignment LHS),
	/// populated at the start of each dynamic body to detect implicit-cell references.
	static_var_position: HashMap<String, (usize, ir::Type)>,
	/// For each variable currently slated for inline expansion, the RHS
	/// expression to generate at its (single) use site. Populated at the
	/// start of each body before code generation; consulted in
	/// `generate(Variable)` and cleared (per entry) on use.
	inline_exp: HashMap<String, &'ast Expression>,
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
	Module { member_index: usize, args: Vec<MidiChannelArg> },
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
			precompiled_callees: Vec<Vec<*const PrecompiledMember>>)
			-> CodeGenerator<'ast, 'comp, 'names> {
		let member_count = callees.len();
		CodeGenerator {
			names,
			compiler,
			signatures,
			stored_widths,
			callees,
			precompiled_callees,

			live: vec![false; member_count],
			live_precompiled: HashSet::new(),
			function_proc_id: vec![0; member_count],
			static_proc_id: vec![0; member_count],
			dynamic_proc_id: vec![0; member_count],
			precompiled_proc_ids: HashMap::new(),
			member_for_id: vec![],
			current_member_index: 0,
			current_kind: MemberKind::Module,
			current_scope: None,

			procedures: vec![],
			code: vec![],
			stack_height: 0,

			next_stack_index: 0,
			stack_index: HashMap::new(),
			static_var_position: HashMap::new(),
			inline_exp: HashMap::new(),
			repetition_depth: 0,
			module_call: vec![],
			track_order: vec![vec![]; member_count],
			update_stack: vec![],
		}
	}

	fn retrieve_width(&self, exp: &Expression) -> Option<Width> {
		self.stored_widths.get(&&raw const *exp).copied()
	}

	fn unsupported(&mut self, loc: &dyn Location, what: &str) {
		self.compiler.report_error(loc, format!("Not supported yet: {}.", what));
	}

	fn emit(&mut self, code: &[Instruction]) {
		for &inst in code {
			let (popped, pushed) = match inst {
				Instruction::Call(id, ..) => {
					let (ref member, scope) = self.member_for_id[id as usize];
					let (inputs, outputs) = match &member.definition {
						MemberDefinition::Declaration { member_index } => {
							let FullSignature { inputs, outputs, .. } = &self.signatures[*member_index];
							(&inputs[..], &outputs[..])
						},
						MemberDefinition::Precompiled { member } => {
							(member.inputs(), member.outputs())
						},
						MemberDefinition::BuiltIn { .. } => {
							panic!("Call of built-in member");
						},
					};
					match member.kind {
						MemberKind::Module => {
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
						MemberKind::Function => {
							(inputs.len(), outputs.len())
						},
						MemberKind::Instrument => {
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

	fn convert_midi_channel(&self, current_member_index: usize, channel: &MidiChannel) -> MidiChannelArg {
		match channel {
			&MidiChannel::Value { channel } => MidiChannelArg::Value { channel },
			MidiChannel::Named { name } => MidiChannelArg::Input {
				index: self.names.lookup_midi_input(current_member_index, &name.text).unwrap()
			},
		}
	}

	fn resolve_midi_channel_arg(&self, channel: &MidiChannelArg, inputs: &Vec<MidiChannelArg>) -> usize {
		match channel {
			MidiChannelArg::Value { channel } => channel - 1,
			MidiChannelArg::Input { index } => self.resolve_midi_channel_arg(&inputs[*index], inputs),
		}
	}

	fn compute_track_order_inner(&mut self, member_index: usize, inputs: &Vec<MidiChannelArg>, track_order: &mut Vec<usize>) {
		for node in self.track_order[member_index].clone() {
			match node {
				TrackOrderNode::Instrument { channel } => {
					track_order.push(self.resolve_midi_channel_arg(&channel, inputs));
				},
				TrackOrderNode::Module { member_index, args  } => {
					self.compute_track_order_inner(member_index, &args, track_order);
				},
			}
		}
	}

	pub fn generate_code_for_program(&mut self, program: &'ast Program, main_index: usize) -> Result<(), CompileError> {
		// Compute liveness
		self.propagate_liveness(main_index);

		self.init_proc_id(program);
		for id in 0..self.member_for_id.len() {
			let (ref member, scope) = self.member_for_id[id];
			let proc = match member.definition {
				MemberDefinition::Declaration { member_index } => {
					self.generate_code_for_member(program, member_index, scope)?
				},
				MemberDefinition::Precompiled { member } => {
					let (kind, index) = match scope {
						None => (ir::ProcedureKind::Function, 0),
						Some(Scope::Static) => (ir::ProcedureKind::Module { scope: ir::Scope::Static }, 0),
						Some(Scope::Dynamic) => (ir::ProcedureKind::Module { scope: ir::Scope::Dynamic }, 1),
					};
					ir::Procedure {
						name: member.name().to_string(),
						kind,
						inputs: self.make_type_list(member.inputs().iter().copied(), scope),
						outputs: self.make_type_list(member.outputs().iter().copied(), scope),
						code: member.instructions()[index].to_vec()
					}
				},
				MemberDefinition::BuiltIn { .. } => {
					panic!("Code generation for built-in member");
				},
			};
			self.procedures.push(proc);
		}
		self.compiler.check_errors()
	}

	fn generate_code_for_member(&mut self,
		program: &'ast Program,
		member_index: usize,
		scope: Option<Scope>,
	) -> Result<ir::Procedure, CompileError> {
		let member = &program.members[member_index];
		let Member { kind, name, inputs, outputs, body, .. } = member;
		self.current_member_index = member_index;
		self.current_kind = *kind;
		self.current_scope = scope;
		let proc_kind;
		let proc_inputs;
		let proc_outputs;
		match kind {
			MemberKind::Module => {
				if scope == Some(Scope::Dynamic) {
					proc_kind = ir::ProcedureKind::Module { scope: ir::Scope::Dynamic };
					proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Dynamic));
					proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Dynamic));
					self.initialize_stack(inputs, Some(Scope::Dynamic), false);
					self.generate_dynamic_body(inputs, body, outputs, false)?;
					let stack_adjust = self.stack_adjust_from_outputs(outputs);
					self.adjust_stack(&stack_adjust[..], self.stack_height);
				} else {
					proc_kind = ir::ProcedureKind::Module { scope: ir::Scope::Static };
					proc_inputs = self.make_proc_type_list(inputs, Some(Scope::Static));
					proc_outputs = self.make_proc_type_list(outputs, Some(Scope::Static));
					self.initialize_stack(inputs, Some(Scope::Static), false);
					self.generate_static_body(body, outputs)?;
					self.adjust_stack(&[], self.stack_height);
				}
			},
			MemberKind::Function => {
				proc_kind = ir::ProcedureKind::Function;
				proc_inputs = self.make_proc_type_list(inputs, None);
				proc_outputs = self.make_proc_type_list(outputs, None);
				self.initialize_stack(inputs, None, false);
				let forbidden: HashSet<String> = outputs.items.iter()
					.map(|i| i.name.text.clone()).collect();
				self.inline_exp = self.compute_inline_exp(
					body,
					None,
					&forbidden,
				);
				for statement in body {
					self.generate_code_for_statement(statement)?;
				}
				let stack_adjust = self.stack_adjust_from_outputs(outputs);
				self.adjust_stack(&stack_adjust[..], self.stack_height);
			},
			MemberKind::Instrument => {
				// Extra implicit input for accumulating instrument outputs.
				let mut real_inputs = inputs.clone();
				real_inputs.items.push(PatternItem {
					name: Id {
						text: "#acc#".to_string(), before: outputs.before,
					},
					item_type: type_spec!(dynamic stereo number),
				});
				let autokill_key = self.names.autokill_key(member);
				if scope == Some(Scope::Dynamic) {
					proc_kind = ir::ProcedureKind::Instrument { scope: ir::Scope::Dynamic };
					self.initialize_stack(&real_inputs, Some(Scope::Dynamic), true);
					self.generate_dynamic_body(&real_inputs, body, outputs, true)?;
					// Leave the inputs (including the accumulator) and the output on the stack.
					let mut stack_adjust: Vec<usize> = (0..(inputs.items.len() + 1)).collect();
					stack_adjust.push(self.stack_index[&outputs.items[0].name.text]);
					self.adjust_stack(&stack_adjust, self.stack_height);
					// Run autokill code
					self.emit(code![Call(self.precompiled_proc_ids[&autokill_key][0], None)]);
				} else {
					proc_kind = ir::ProcedureKind::Instrument { scope: ir::Scope::Static };
					self.initialize_stack(&real_inputs, Some(Scope::Static), true);
					self.generate_static_body(body, outputs)?;
					// Init autokill
					self.emit(code![Call(self.precompiled_proc_ids[&autokill_key][1], None)]);
					// Leave only the inputs (including the accumulator) on the stack.
					self.adjust_stack(&[], self.stack_height - real_inputs.items.len());
				}
				proc_inputs = self.make_proc_type_list(&real_inputs, None);
				proc_outputs = self.make_proc_type_list(&real_inputs, None);
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

	fn propagate_liveness(&mut self, member_index: usize) {
		if !self.live[member_index] {
			self.live[member_index] = true;
			for &callee in &self.callees[member_index].clone() {
				self.propagate_liveness(callee);
			}
			for precompiled_callee in &self.precompiled_callees[member_index] {
				self.live_precompiled.insert(*precompiled_callee);
			}
		}
	}

	fn assign_ids(&mut self, program: &Program, pred: &dyn Fn(MemberKind, &str) -> bool) {
		for (member_index, member) in program.members.iter().enumerate() {
			if self.live[member_index] && pred(member.kind, member.name.text.as_str()) {
				let mut push_id = |proc_id: &mut Vec<u16>, scope: Option<Scope>| {
					proc_id[member_index] = self.member_for_id.len() as u16;
					let member_ref = MemberRef {
						context: member.context,
						kind: member.kind,
						definition: MemberDefinition::Declaration { member_index }
					};
					self.member_for_id.push((member_ref, scope));
				};
				match member.kind {
					MemberKind::Function => {
						push_id(&mut self.function_proc_id, None);
					},
					MemberKind::Module | MemberKind::Instrument => {
						push_id(&mut self.dynamic_proc_id, Some(Scope::Dynamic));
						push_id(&mut self.static_proc_id, Some(Scope::Static));
					},
				};
			}
		}
	}

	fn assign_precompiled_ids(&mut self,
		procs: &'static [PrecompiledMember],
		kind: MemberKind,
		scopes: &[Option<Scope>]
	) {
		for member in procs {
			if self.live_precompiled.contains(&&raw const *member) {
				for &scope in scopes {
					self.precompiled_proc_ids.entry(member).or_default().push(self.member_for_id.len() as u16);
					let member_ref = MemberRef {
						context: member.context(),
						kind: kind,
						definition: MemberDefinition::Precompiled { member }
					};
					self.member_for_id.push((member_ref, scope));
				};
			}
		}
	}

	fn init_proc_id(&mut self, program: &Program) {
		// Assign procedure IDs to all members in this order:
		// main, instruments, modules (except main), functions.
		self.assign_ids(program, &|_, name| name == "main");
		self.assign_ids(program, &|kind, _| kind == MemberKind::Instrument);
		self.assign_precompiled_ids(PRECOMPILED_MODULES, MemberKind::Module, &[Some(Scope::Dynamic), Some(Scope::Static)]);
		self.assign_ids(program, &|kind, name| kind == MemberKind::Module && name != "main");
		self.assign_precompiled_ids(PRECOMPILED_FUNCTIONS, MemberKind::Function, &[None]);
		self.assign_ids(program, &|kind, _| kind == MemberKind::Function);
	}

	fn inlineable_item<'a>(&self, node: &'a Pattern) -> Option<&'a PatternItem> {
		if let [item] = node.items.as_slice() && item.name.text != "_" {
			Some(item)
		} else {
			None
		}
	}

	/// Find all single-LHS assignments whose item scope matches `scope`.
	/// Returns `(name, &exp)` pairs in source order.
	fn collect_inline_candidates(&self,
		body: &'ast Vec<Statement>,
		scope: Option<Scope>,
	) -> Vec<(String, &'ast Expression)> {
		let mut out = vec![];
		for Statement::Assign { node, exp } in body {
			if let Some(item) = self.inlineable_item(node) {
				if item.item_type.scope == scope {
					out.push((item.name.text.clone(), exp));
				}
			}
		}
		out
	}

	/// Walk all statements and accumulate variable-reference counts and
	/// loop-nesting info for `tracked` names, counting only uses evaluated
	/// in the `scope` phase. Each statement is scanned with its own scope
	/// as the starting context; `scan_exp` switches context at phase
	/// boundaries, so e.g. the static init arg of a `cell` inside a
	/// dynamic statement is counted as a static-phase use.
	fn count_uses(&self,
		body: &'ast Vec<Statement>,
		tracked: &HashSet<String>,
		scope: Option<Scope>,
	) -> (HashMap<String, usize>, HashSet<String>) {
		let mut counts = HashMap::new();
		let mut in_loop = HashSet::new();
		let mut order = vec![];
		for Statement::Assign { node, exp } in body {
			let stmt_scope = node.items.first().and_then(|i| i.item_type.scope);
			self.scan_exp(exp, tracked, scope, stmt_scope, &mut counts, &mut in_loop, &mut order, 0);
		}
		(counts, in_loop)
	}

	/// Build the `inline_exp` map for a single body. A candidate
	/// (single-LHS, non-`_` assignment matching `scope`) is
	/// inlineable when its name is referenced exactly once in scanned
	/// statements, the use is not inside a `For`, and the name is not in
	/// `forbidden`.
	fn compute_inline_exp(&self,
		body: &'ast Vec<Statement>,
		scope: Option<Scope>,
		forbidden: &HashSet<String>,
	) -> HashMap<String, &'ast Expression> {
		let candidates = self.collect_inline_candidates(body, scope);
		let candidate_names: HashSet<String> = candidates.iter()
			.filter(|(n, _)| !forbidden.contains(n))
			.map(|(n, _)| n.clone())
			.collect();
		let (counts, in_loop) = self.count_uses(body, &candidate_names, scope);

		let mut out = HashMap::new();
		for (name, exp) in candidates {
			if forbidden.contains(&name) { continue; }
			if counts.get(&name).copied() == Some(1) && !in_loop.contains(&name) {
				out.insert(name, exp);
			}
		}
		out
	}

	/// Names of static-scope LHSs that the static body could inline. Used
	/// in two places: to populate `inline_exp` when generating the static
	/// body, and to skip pos increments when building
	/// `static_var_position` for the dynamic body so the runtime stack
	/// layout stays aligned with the recorded positions.
	fn compute_static_inlineable(&self,
		body: &'ast Vec<Statement>,
		outputs: &'ast Pattern,
	) -> HashMap<String, &'ast Expression> {
		// Collect static-scope single-LHS candidate names.
		let static_candidates: Vec<String> = self.collect_inline_candidates(body, Some(Scope::Static))
			.into_iter().map(|(n, _)| n).collect();
		let static_lhs_set: HashSet<String> = static_candidates.iter().cloned().collect();

		// A static var that is referenced from the dynamic body needs its
		// stack slot for the implicit-cell init, so it cannot be inlined
		// here.
		let (dyn_counts, _) = self.count_uses(body, &static_lhs_set, Some(Scope::Dynamic));
		let mut forbidden: HashSet<String> = dyn_counts.into_keys().collect();
		// Outputs are returned to the caller via a stack slot — they
		// can't be inlined either.
		for item in &outputs.items {
			forbidden.insert(item.name.text.clone());
		}

		self.compute_inline_exp(
			body,
			Some(Scope::Static),
			&forbidden,
		)
	}

	fn generate_static_body(&mut self,
		body: &'ast Vec<Statement>,
		outputs: &'ast Pattern,
	) -> Result<(), CompileError> {
		self.inline_exp = self.compute_static_inlineable(body, outputs);
		for statement in body {
			if statement_scope(statement) == Some(Scope::Static) {
				self.generate_code_for_statement(statement)?;
			}
		}
		self.generate_static_module_calls(&self.module_call.clone());
		Ok(())
	}

	fn generate_static_module_calls(&mut self, module_call: &Vec<ModuleCall<'ast>>) {
		for call in module_call {
			match call {
				&ModuleCall::ImplicitCell { stack_index, cell_type } => {
					let offset = self.stack_height - stack_index - 1;
					self.emit(code![StackLoad(offset as u16), CellInit(cell_type)]);
				},
				&ModuleCall::Init { kind, value, cell_type } => {
					self.generate(value);
					match kind {
						StateKind::Cell => {
							self.emit(code![CellInit(cell_type)]);
						},
						StateKind::Delay => {
							self.emit(code![BufferAlloc(cell_type.width), CellInit(cell_type)]);
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

	fn generate_dynamic_body(&mut self,
		inputs: &Pattern,
		body: &'ast Vec<Statement>,
		outputs: &'ast Pattern,
		all_scopes: bool,
	) -> Result<(), CompileError> {
		self.module_call.clear();
		self.static_var_position.clear();
		debug_assert!(self.update_stack.is_empty());

		// Recompute the static-side inlineable set. Static-inlineable
		// assignments are skipped at runtime in the static procedure (no
		// stack slot pushed), so their positions in the static stack
		// layout vanish and subsequent static-scope LHSs shift down by
		// one. We must mirror that skip here so `static_var_position`
		// stays aligned with what `generate_static_module_calls` sees at
		// runtime.
		let static_inlineable: HashSet<String> = self.compute_static_inlineable(body, outputs)
			.into_keys().collect();

		// Compute static-phase positions for static-scope inputs and static-scope
		// assignment LHSs in source order. Used during the dynamic walk to detect
		// implicit-cell references.
		let mut pos = 0usize;
		for item in &inputs.items {
			if item.item_type.scope == Some(Scope::Static) {
				if item.name.text != "_" {
					self.static_var_position.insert(item.name.text.clone(), (pos, item.item_type.to_ir()));
				}
				pos += 1;
			} else if all_scopes {
				pos += 1;
			}
		}
		for Statement::Assign { node, .. } in body {
			let stmt_scope = node.items.first().and_then(|item| item.item_type.scope);
			if stmt_scope == Some(Scope::Static) {
				if self.inlineable_item(node)
					.filter(|item| static_inlineable.contains(&item.name.text))
					.is_none()
				{
					for item in &node.items {
						if item.name.text != "_" {
							self.static_var_position.insert(item.name.text.clone(), (pos, item.item_type.to_ir()));
						}
						pos += 1;
					}
				}
			}
		}

		// Inlineable dynamic-scope assignments. Populated for use by
		// `generate(Variable)` while emitting the dynamic body.
		let forbidden_dyn: HashSet<String> = outputs.items.iter()
			.map(|i| i.name.text.clone()).collect();
		self.inline_exp = self.compute_inline_exp(
			body,
			Some(Scope::Dynamic),
			&forbidden_dyn,
		);

		let hoisted = self.scan_dynamic_for_hoist(body, outputs);

		for &(ref name, static_pos, cell_type) in &hoisted {
			self.module_call.push(ModuleCall::ImplicitCell { stack_index: static_pos, cell_type });
			self.stack_index.insert(name.clone(), self.stack_height);
			self.emit(code![CellRead(cell_type)]);
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

	fn scan_dynamic_for_hoist(&self,
		body: &'ast Vec<Statement>,
		outputs: &'ast Pattern,
	) -> Vec<(String, usize, ir::Type)> {
		let tracked: HashSet<String> = self.static_var_position.keys().cloned().collect();
		let mut counts: HashMap<String, usize> = HashMap::new();
		let mut in_loop: HashSet<String> = HashSet::new();
		let mut order: Vec<String> = vec![];
		for Statement::Assign { node, exp } in body {
			if node.items.iter().any(|item| item.item_type.scope == Some(Scope::Dynamic)) {
				self.scan_exp(exp, &tracked, Some(Scope::Dynamic), Some(Scope::Dynamic), &mut counts, &mut in_loop, &mut order, 0);
			}
		}

		let mut hoisted: Vec<(String, usize, ir::Type)> = order.iter()
			.filter(|n| counts[*n] > 1 || in_loop.contains(*n))
			.map(|n| {
				let (pos, cell_type) = self.static_var_position[n];
				(n.clone(), pos, cell_type)
			})
			.collect();

		for item in &outputs.items {
			if let Some(&(pos, cell_type)) = self.static_var_position.get(&item.name.text) {
				if !hoisted.iter().any(|(n, _, _)| n == &item.name.text) {
					hoisted.push((item.name.text.clone(), pos, cell_type));
				}
			}
		}
		hoisted
	}

	/// Count uses of `tracked` names in `exp` that are evaluated in the
	/// `phase` phase. `context` is the phase the expression itself is
	/// evaluated in (its statement's scope at the top level, or `None`
	/// inside a function, where everything is single-phase); a use is
	/// counted only when `context == phase`. Recursion switches context
	/// at phase boundaries: the static args of built-in and module calls,
	/// `For` counts and `BufferInit` lengths/args are static-phase even
	/// inside a dynamic statement.
	fn scan_exp(&self,
		exp: &'ast Expression,
		tracked: &HashSet<String>,
		phase: Option<Scope>,
		context: Option<Scope>,
		counts: &mut HashMap<String, usize>,
		in_loop: &mut HashSet<String>,
		order: &mut Vec<String>,
		loop_depth: usize,
	) {
		// Inside a function (`context == None`), sub-expressions stay
		// single-phase; otherwise they get the given phase.
		let sub_context = |scope| context.and(scope);
		use Expression::*;
		match exp {
			Number { .. } | Bool { .. } | TupleIndex { .. } => {},
			Variable { name } => {
				if context == phase && tracked.contains(&name.text) {
					let count = counts.entry(name.text.clone()).or_insert(0);
					if *count == 0 {
						order.push(name.text.clone());
					}
					*count += 1;
					if loop_depth > 0 {
						in_loop.insert(name.text.clone());
					}
				}
			},
			UnOp { exp, .. } => self.scan_exp(exp, tracked, phase, context, counts, in_loop, order, loop_depth),
			BinOp { left, right, .. } => {
				self.scan_exp(left, tracked, phase, context, counts, in_loop, order, loop_depth);
				self.scan_exp(right, tracked, phase, context, counts, in_loop, order, loop_depth);
			},
			Conditional { condition, then, otherwise } => {
				self.scan_exp(condition, tracked, phase, context, counts, in_loop, order, loop_depth);
				self.scan_exp(then, tracked, phase, context, counts, in_loop, order, loop_depth);
				self.scan_exp(otherwise, tracked, phase, context, counts, in_loop, order, loop_depth);
			},
			Call { name, args, .. } => {
				match self.names.lookup_member(&name.text) {
					Some(MemberRef { kind, definition, .. }) => {
						use MemberKind::*;
						use MemberDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								// `cell`/`delay` update arg is dynamic; their initial value
								// arg is static. `dyndelay` has dynamic update + dynamic
								// offset, with a static initial value.
								let static_args: &[usize] = match name.text.as_str() {
									"cell"     => &[1],
									"delay"    => &[1],
									"dyndelay" => &[2],
									_ => panic!("Unknown built-in module"),
								};
								for (i, arg) in args.iter().enumerate() {
									let arg_scope = if static_args.contains(&i) { Scope::Static } else { Scope::Dynamic };
									self.scan_exp(arg, tracked, phase, sub_context(Some(arg_scope)), counts, in_loop, order, loop_depth);
								}
							},
							(Module, Precompiled { member }) => {
								let inputs = member.inputs();
								for (arg, input_type) in args.iter().zip(inputs) {
									self.scan_exp(arg, tracked, phase, sub_context(input_type.scope), counts, in_loop, order, loop_depth);
								}
							},
							(Module, Declaration { member_index }) => {
								let inputs = &self.signatures[*member_index].inputs;
								for (arg, input_type) in args.iter().zip(inputs) {
									self.scan_exp(arg, tracked, phase, sub_context(input_type.scope), counts, in_loop, order, loop_depth);
								}
							},
							(Function, _) | (Instrument, _) => {
								for arg in args {
									self.scan_exp(arg, tracked, phase, context, counts, in_loop, order, loop_depth);
								}
							},
						}
					},
					None => panic!("Member not found"),
				}
			},
			Tuple { elements, .. } => {
				for el in elements {
					self.scan_exp(el, tracked, phase, context, counts, in_loop, order, loop_depth);
				}
			},
			Merge { left, right, .. } => {
				self.scan_exp(left, tracked, phase, context, counts, in_loop, order, loop_depth);
				self.scan_exp(right, tracked, phase, context, counts, in_loop, order, loop_depth);
			},
			BufferIndex { exp, index, .. } => {
				self.scan_exp(exp, tracked, phase, context, counts, in_loop, order, loop_depth);
				self.scan_exp(index, tracked, phase, context, counts, in_loop, order, loop_depth);
			},
			For { count, body, .. } => {
				// The loop counter is evaluated once before the loop, in the static
				// phase (it lives in the surrounding `ModuleCall::For`). The body
				// is evaluated N times in the current phase.
				self.scan_exp(count, tracked, phase, sub_context(Some(Scope::Static)), counts, in_loop, order, loop_depth);
				self.scan_exp(body, tracked, phase, context, counts, in_loop, order, loop_depth + 1);
			},
			BufferInit { length, body, .. } => {
				// In the dynamic phase, BufferInit just CellReads its precomputed
				// value. `length` and the inner-call args are evaluated in the
				// static phase.
				self.scan_exp(length, tracked, phase, sub_context(Some(Scope::Static)), counts, in_loop, order, loop_depth);
				if let Expression::Call { args, .. } = &**body {
					for arg in args {
						self.scan_exp(arg, tracked, phase, sub_context(Some(Scope::Static)), counts, in_loop, order, loop_depth);
					}
				}
			},
			BufferLiteral { elements, .. } => {
				for el in elements {
					self.scan_exp(el, tracked, phase, context, counts, in_loop, order, loop_depth);
				}
			},
			Expand { exp, .. } => self.scan_exp(exp, tracked, phase, context, counts, in_loop, order, loop_depth),
		}
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
		if self.inlineable_item(node)
			.filter(|item| self.inline_exp.contains_key(&item.name.text))
			.is_none()
		{
			self.generate(exp);
			self.add_stack_indices(node, None, false, false);
		}
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
						if let Some(exp) = self.inline_exp.remove(&name.text) {
							// Single-use inlined assignment: emit the RHS here.
							self.generate(exp);
						} else if let Some(&(static_pos, cell_type)) = self.static_var_position.get(&name.text) {
							// Inline (single-use, non-loop, non-output) implicit cell.
							self.module_call.push(ModuleCall::ImplicitCell { stack_index: static_pos, cell_type });
							self.emit(code![CellRead(cell_type)]);
						} else {
							match self.names.lookup_variable(self.current_member_index, &name.text) {
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
			Call { channels, name, args, .. } => {
				match self.names.lookup_member(&name.text) {
					Some(MemberRef { kind, definition, .. }) => {
						use MemberKind::*;
						use MemberDefinition::*;
						match (kind, definition) {
							(Module, BuiltIn { .. }) => {
								if self.repetition_depth > 0 {
									self.unsupported(exp, "Built-in module in repetition body");
								}
								let width = self.retrieve_width(exp).unwrap().to_ir();
								match name.text.as_str() {
									"cell" => {
										let cell_type = ir::Type { width, value_type: ir::ValueType::Number };
										self.module_call.push(ModuleCall::Init { kind: StateKind::Cell, value: &args[1], cell_type });
										self.emit(code![CellPush(cell_type)]);
										self.update_stack.push((StateKind::Cell, &args[0]));
									},
									"delay" => {
										let cell_type = ir::Type { width, value_type: ir::ValueType::Buffer };
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[1], cell_type });
										self.emit(code![CellPush(cell_type), BufferLoad]);
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									"dyndelay" => {
										let cell_type = ir::Type { width, value_type: ir::ValueType::Buffer };
										self.module_call.push(ModuleCall::Init { kind: StateKind::Delay, value: &args[2], cell_type });
										self.emit(code![CellPush(cell_type)]);
										self.generate(&args[1]);
										self.emit(code![BufferLoadWithOffset]);
										self.update_stack.push((StateKind::Delay, &args[0]));
									},
									_ => panic!("Unknown built-in module"),
								}
							},
							(Module, Precompiled { member }) => {
								let inputs = member.inputs();
								for (arg, input_type) in args.iter().zip(inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.generate(arg);
									}
								}
								let key = &raw const **member;
								self.module_call.push(ModuleCall::Call {
									inputs: inputs.to_vec(),
									static_proc_id: self.precompiled_proc_ids[&key][1],
									generic_width: self.retrieve_width(exp),
									args,
								});
								let proc_id = self.precompiled_proc_ids[&key][0];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Module, Declaration { member_index }) => {
								let current_member_index = self.current_member_index;
								let FullSignature { context, inputs, .. } = &self.signatures[*member_index];
								let context = *context;
								let inputs = inputs.clone();
								for (arg, input_type) in args.iter().zip(&inputs) {
									if input_type.scope == Some(Scope::Dynamic) {
										self.generate(arg);
									}
								}
								self.module_call.push(ModuleCall::Call {
									inputs: inputs,
									static_proc_id: self.static_proc_id[*member_index],
									generic_width: self.retrieve_width(exp),
									args,
								});
								if context == Context::Global {
									let resolved_args: Vec<MidiChannelArg> = channels.iter()
										.map(|channel| self.convert_midi_channel(current_member_index, channel))
										.collect();
									let node = TrackOrderNode::Module { member_index: *member_index, args: resolved_args };
									self.track_order[current_member_index].push(node);
								}
								let proc_id = self.dynamic_proc_id[*member_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Function, BuiltIn { code, .. }) => {
								for arg in args {
									self.generate(arg);
								}
								self.emit(code);
							},
							(Function, Precompiled { member }) => {
								for arg in args {
									self.generate(arg);
								}
								let key = &raw const **member;
								let proc_id = self.precompiled_proc_ids[&key][0];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Function, Declaration { member_index }) => {
								for arg in args {
									self.generate(arg);
								}
								let proc_id = self.function_proc_id[*member_index];
								self.emit(code![Call(proc_id, self.retrieve_width(exp).to_ir())]);
							},
							(Instrument, Declaration { member_index }) => {
								let current_member_index = self.current_member_index;
								let static_proc_id = self.static_proc_id[*member_index];
								let dynamic_proc_id = self.dynamic_proc_id[*member_index];

								let FullSignature { inputs, outputs, .. } = &self.signatures[*member_index];
								let width = outputs.first().unwrap().width.unwrap();
								let (in_count, out_count) = (inputs.len() + 1, outputs.len());
								let channel = self.convert_midi_channel(current_member_index, &channels[0]);
								let node = TrackOrderNode::Instrument { channel };
								self.track_order[current_member_index].push(node);
								for arg in args {
									self.generate(arg);
								}
								self.emit(code![Constant(0)]);
								self.expand(width);
								self.emit(code![PlayInstrument(static_proc_id, dynamic_proc_id)]);
								let stack_adjust: Vec<usize> = (in_count - out_count .. in_count).collect();
								self.adjust_stack(&stack_adjust[..], in_count);
							},
							(Instrument, _) => panic!("Built-in instrument"),
						}
					},
					None => panic!("Member not found"),
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
			For { name, count, body, combinator, .. } => {
				let module_call_temp = replace(&mut self.module_call, vec![]);
				self.repetition_depth += 1;
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
				self.repetition_depth -= 1;
				let nested_calls = replace(&mut self.module_call, module_call_temp);
				self.module_call.push(ModuleCall::For { name, count, nested_calls });
			},
			BufferInit { width, length, body, .. } => {
				match self.current_scope {
					Some(Scope::Static) => {
						let Expression::Call { ref name, ref args, .. } = **body else {
							panic!("Buffer initialization must be a call");
						};
						let member_index = match self.names.lookup_member(&name.text) {
							Some(MemberRef { definition: MemberDefinition::Declaration { member_index }, .. }) => member_index,
							_ => panic!("Buffer initialization must be a call"),
						};

						let static_proc_id = self.static_proc_id[*member_index];
						let dynamic_proc_id = self.dynamic_proc_id[*member_index];

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
						let cell_type = ir::Type { width: width.unwrap().to_ir(), value_type: ir::ValueType::Buffer };
						self.module_call.push(ModuleCall::Init {
							kind: StateKind::Cell,
							value: exp,
							cell_type,
						});
						self.emit(code![CellRead(cell_type)]);
					},
					None => panic!("Buffer initialization in a function"),
				}
			},
			BufferLiteral { elements, .. } => {
				let buffer_width = self.retrieve_width(exp).unwrap();
				self.emit(code![Constant((elements.len() as f32).to_bits())]);
				self.emit(code![BufferAlloc(buffer_width.to_ir())]);
				for element in elements {
					self.generate(element);
					self.emit(code![BufferStoreAndStep]);
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
			Width::Stereo => self.emit(code![Expand(ir::Width::Stereo)]),
			Width::Generic => self.emit(code![Expand(ir::Width::Generic)]),
		}
	}
}
