
use std::collections::{HashMap, HashSet};

use ir::Instruction;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::{Compiler, CompileError, Location, PosRange};

/// Mapping of names in the program to their definitions.
#[derive(Clone, Debug)]
pub struct Names {
	parameters: HashMap<String, VariableRef>,
	members: HashMap<String, MemberRef>,
	midi: Vec<HashMap<String, (usize, PosRange)>>,
	variables: Vec<HashMap<String, VariableRef>>,
	statement_order: Vec<Vec<usize>>,
	combinators: HashMap<String, Combinator>,
}

/// A reference to a named entry in a pattern.
#[derive(Clone, Copy, Debug)]
pub enum VariableRef {
	Parameter {index: usize },
	Input { index: usize },
	Node { body_index: usize, tuple_index: usize },
	For { variable_pos: PosRange },
}

/// A reference to a top-level module, function or instrument.
#[derive(Clone, Debug)]
pub struct MemberRef {
	pub context: Context,
	pub kind: MemberKind,
	pub definition: MemberDefinition,
}

/// Is this a built-in definition or declared in the program?
#[derive(Clone, Debug)]
pub enum MemberDefinition {
	BuiltIn {
		sig: &'static Signature<'static>,
		code: &'static [Instruction],
	},
	Precompiled { member: &'static PrecompiledMember },
	Declaration { member_index: usize },
}

/// Signature of a member or operator.
#[derive(Clone, Debug)]
pub struct Signature<'sig> {
	pub inputs: &'sig [Type],
	pub outputs: &'sig [Type],
}

// Description of a repetition combinator
#[derive(Clone, Debug)]
pub struct Combinator {
	pub neutral: f32,
	pub code: &'static [Instruction],
}

impl Names {
	/// Find all named entities in the program and build a map for locating them.
	pub fn find(program: &Program, compiler: &mut Compiler) -> Result<Names, CompileError> {
		let mut names = Names {
			parameters: HashMap::new(),
			members: HashMap::new(),
			midi: vec![HashMap::new(); program.members.len()],
			variables: vec![HashMap::new(); program.members.len()],
			statement_order: vec![vec![]; program.members.len()],
			combinators: HashMap::new(),
		};

		// Insert built-in functions and modules
		for &(name, context, ref sig, code) in BUILTIN_FUNCTIONS {
			names.members.insert(name.to_string(), MemberRef {
				context,
				kind: MemberKind::Function,
				definition: MemberDefinition::BuiltIn { sig, code },
			});
		}
		for &(name, context, ref sig) in BUILTIN_MODULES {
			names.members.insert(name.to_string(), MemberRef {
				context,
				kind: MemberKind::Module,
				definition: MemberDefinition::BuiltIn { sig, code: &[] },
			});
		}
		for member in PRECOMPILED_FUNCTIONS {
			names.members.insert(member.name().to_string(), MemberRef {
				context: member.context(),
				kind: MemberKind::Function,
				definition: MemberDefinition::Precompiled { member },
			});
		}
		for member in PRECOMPILED_MODULES {
			names.members.insert(member.name().to_string(), MemberRef {
				context: member.context(),
				kind: MemberKind::Module,
				definition: MemberDefinition::Precompiled { member },
			});
		}
		for &(name, neutral, code) in REPETITION_COMBINATORS {
			names.combinators.insert(name.to_string(), Combinator { neutral, code });
		}

		// Insert parameters
		for (index, param) in program.parameters.iter().enumerate() {
			names.parameters.insert(param.name.text.clone(), VariableRef::Parameter { index });
		}

		// Run through all members in the program
		for (member_index, member) in program.members.iter().enumerate() {
			let member_ref = MemberRef {
				context: member.context,
				kind: member.kind,
				definition: MemberDefinition::Declaration { member_index },
			};
			names.insert_member(program, compiler, &member.name, member_ref);

			// Insert midi inputs
			for (index, name) in member.midi.iter().enumerate() {
				names.insert_midi(compiler, member_index, name, index);
			}

			// Run through all patterns in the member; first the inputs,
			// then the left-hand sides of all assignments.
			for (index, item) in member.inputs.items.iter().enumerate() {
				names.insert_variable(program, compiler, member_index, &item.name,
					VariableRef::Input { index });
			}
			for (body_index, Statement::Assign { node, exp }) in member.body.iter().enumerate() {
				for (tuple_index, item) in node.items.iter().enumerate() {
					names.insert_variable(program, compiler, member_index, &item.name,
						VariableRef::Node { body_index, tuple_index });
				}
				// Add any repetition variables in the right-hand side.
				exp.traverse_pre(&mut |exp| {
					if let Expression::For { name, .. } = exp {
						names.insert_variable(program, compiler, member_index, &name,
							VariableRef::For { variable_pos: PosRange::from(name) });
					}
				});
			}
			names.statement_order[member_index] = names.find_statement_order(compiler, member_index, member);
		}

		compiler.if_no_errors(names)
	}

	fn insert_member(&mut self,
			program: &Program, compiler: &mut Compiler,
			name: &Id, member_ref: MemberRef) {
		self.members.entry(name.text.clone()).and_modify(|existing| {
			match existing.definition {
				MemberDefinition::BuiltIn { .. } | MemberDefinition::Precompiled { .. } => {
					compiler.report_error(name,
						format!("The {} '{}' has the same name as a built-in {}.",
							member_ref.kind, name, existing.kind));
				},
				MemberDefinition::Declaration { member_index } => {
					compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
					let existing_name = &program.members[member_index].name;
					compiler.report_context(existing_name, "Previously defined here.");
				},
			}
		}).or_insert(member_ref);
	}

	fn insert_midi(&mut self,
			compiler: &mut Compiler,
			member_index: usize, name: &Id, index: usize) {
		if name.text == "_" { return; }
		self.midi[member_index].entry(name.text.clone()).and_modify(|_| {
			compiler.report_error(name, format!("Duplicate MIDI input '{}'.", name));
		}).or_insert((index, PosRange::from(name)));
	}

	fn insert_variable(&mut self,
			program: &Program, compiler: &mut Compiler,
			member_index: usize, name: &Id, var_ref: VariableRef) {
		if name.text == "_" { return; }
		self.variables[member_index].entry(name.text.clone()).and_modify(|existing| {
			if let (VariableRef::For { .. }, VariableRef::For { .. }) = (&existing, &var_ref) {
				// Different repetition variables in the same body can have the same name.
				return;
			}
			compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
			let member = &program.members[member_index];
			let loc: &dyn Location = match existing {
				VariableRef::Parameter { index } => &program.parameters[*index].name,
				VariableRef::Input { index } => &member.inputs.items[*index].name,
				VariableRef::Node { body_index, tuple_index } => match member.body[*body_index] {
					Statement::Assign { ref node, .. } => &node.items[*tuple_index].name,
				},
				VariableRef::For { variable_pos } => variable_pos,
			};
			compiler.report_context(loc, "Previously defined here.");
		}).or_insert(var_ref);
	}

	/// Order the statements of a member so that each comes after the statements it
	/// refers to, keeping source order where possible. The first argument of a
	/// built-in module (`cell`, `delay`, `dyndelay`) is the value for the next sample,
	/// evaluated after the whole body, so it may refer to any statement. Other
	/// references must not form a cycle.
	fn find_statement_order(&self, compiler: &mut Compiler, member_index: usize, member: &Member) -> Vec<usize> {
		// For each statement, the statements it refers to, with the referring names.
		let references: Vec<Vec<(usize, &Id)>> = member.body.iter().map(|Statement::Assign { exp, .. }| {
			let mut next_values: HashSet<*const Expression> = HashSet::new();
			exp.traverse_pre(&mut |exp| {
				if let Expression::Call { name, args, .. } = exp
					&& let Some(MemberRef { kind: MemberKind::Module, definition: MemberDefinition::BuiltIn { .. }, .. })
						= self.lookup_member(&name.text)
					&& let Some(next) = args.first()
				{
					next.traverse_pre(&mut |exp| { next_values.insert(exp); });
				}
			});
			let mut statement_references = vec![];
			exp.traverse_pre(&mut |exp| {
				if let Expression::Variable { name } = exp
					&& !next_values.contains(&(exp as *const Expression))
					&& let Some(&VariableRef::Node { body_index, .. }) = self.lookup_variable(member_index, &name.text)
				{
					statement_references.push((body_index, name));
				}
			});
			statement_references
		}).collect();

		let mut visits = vec![Visit::New; member.body.len()];
		let mut path = vec![];
		let mut order = vec![];
		for body_index in 0..member.body.len() {
			visit_statement(compiler, &references, body_index, &mut visits, &mut path, &mut order);
		}
		order
	}

	/// Look up a module, function or instrument by name.
	pub fn lookup_member(&self, name: &String) -> Option<&MemberRef> {
		self.members.get(name)
	}

	// Lookup a precompiled module or function by name.
	pub fn lookup_precompiled(&self, name: &str) -> Option<&'static PrecompiledMember> {
		match self.lookup_member(&name.to_string()) {
			Some(MemberRef { definition: MemberDefinition::Precompiled { member }, .. }) => Some(member),
			_ => None,
		}
	}

	/// Look up a MIDI input by name inside a specific member.
	pub fn lookup_midi_input(&self, member_index: usize, name: &String) -> Option<usize> {
		self.midi[member_index].get(name).map(|(index, _)| *index)
	}

	/// The indices of the statements of a member, in the order to evaluate them.
	pub fn statement_order(&self, member_index: usize) -> &[usize] {
		&self.statement_order[member_index]
	}

	/// Look up a variable by name inside a specific member.
	pub fn lookup_variable(&self, member_index: usize, name: &String) -> Option<&VariableRef> {
		self.variables[member_index].get(name).or_else(|| self.parameters.get(name))
	}

	pub fn lookup_combinator(&self, combinator: &String) -> Option<&Combinator> {
		self.combinators.get(combinator)
	}

	pub fn combinator_list(&self) -> String {
		let mut combinators: Vec<String> = self.combinators.keys().cloned().collect();
		combinators.sort();
		let mut list = format!("'{}'", combinators[0]);
		for i in 1 .. combinators.len() - 1 {
			list += &format!(", '{}'", combinators[i]);
		}
		list += &format!(" and '{}'", combinators.last().unwrap());
		list
	}

	// The name of the autokill precompiled module for a given instrument.
	pub fn autokill_key(&self, member: &Member) -> *const PrecompiledMember {
		assert!(member.kind == MemberKind::Instrument);
		let autokill_name = match member.outputs.items.first().unwrap().item_type.width {
			Some(Width::Mono) => "$autokill_mono",
			Some(Width::Stereo) => "$autokill_stereo",
			_ => unreachable!("Missing instrument output width"),
		};
		&raw const *self.lookup_precompiled(&autokill_name).unwrap()
	}
}

#[derive(Clone, Copy, PartialEq)]
enum Visit { New, Active, Done }

/// Depth-first: put the statements that a statement refers to before it in `order`.
/// `path` holds the statements being visited, each with its reference to the next.
fn visit_statement<'ast>(
		compiler: &mut Compiler,
		references: &[Vec<(usize, &'ast Id)>],
		body_index: usize,
		visits: &mut [Visit],
		path: &mut Vec<(usize, &'ast Id)>,
		order: &mut Vec<usize>) {
	if visits[body_index] != Visit::New { return; }
	visits[body_index] = Visit::Active;
	for &(target, name) in &references[body_index] {
		match visits[target] {
			Visit::New => {
				path.push((body_index, name));
				visit_statement(compiler, references, target, visits, path, order);
				path.pop();
			},
			Visit::Active => {
				// The references from `target` along the path, and back by `name`, form a cycle.
				let start = path.iter().position(|&(index, _)| index == target).unwrap_or(path.len());
				let cycle: Vec<&Id> = path[start..].iter().map(|&(_, name)| name).chain([name]).collect();
				compiler.report_error(cycle[0], format!(
					"'{}' depends on itself. A cycle must go through the first argument of a cell or delay.", name));
				for pair in cycle.windows(2) {
					compiler.report_context(pair[1], format!("'{}' depends on '{}' here.", pair[0], pair[1]));
				}
			},
			Visit::Done => {},
		}
	}
	visits[body_index] = Visit::Done;
	order.push(body_index);
}
