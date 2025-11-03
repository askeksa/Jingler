
use std::collections::HashMap;

use program::instructions::Instruction;

use crate::ast::*;
use crate::builtin::*;
use crate::compiler::{Compiler, CompileError, Location, PosRange};

/// Mapping of names in the program to their definitions.
#[derive(Clone, Debug)]
pub struct Names<'ast> {
	parameters: HashMap<&'ast str, VariableRef>,
	procedures: HashMap<&'ast str, ProcedureRef>,
	variables: Vec<HashMap<&'ast str, VariableRef>>,
	combinators: HashMap<&'ast str, Combinator>,
}

/// A reference to a named entry in a pattern.
#[derive(Clone, Debug)]
pub struct VariableRef {
	pub kind: VariableKind,
	pub tuple_index: usize,
}

/// Location of the pattern this variable occurs in: input or node.
#[derive(Clone, Copy, Debug)]
pub enum VariableKind {
	Parameter,
	Input,
	Node { body_index: usize },
	For { variable_pos: PosRange },
}

/// A reference to a top-level module, function or instrument.
#[derive(Clone, Debug)]
pub struct ProcedureRef {
	pub kind: ProcedureKind,
	pub definition: ProcedureDefinition,
}

/// Is this a built-in definition or declared in the program?
#[derive(Clone, Debug)]
pub enum ProcedureDefinition {
	BuiltIn {
		sig: &'static Signature<'static>,
		code: &'static [Instruction],
	},
	Precompiled { proc: &'static PrecompiledProcedure },
	Declaration { proc_index: usize },
}

/// Signature of a procedure or operator.
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

impl<'ast> Names<'ast> {
	/// Find all named entities in the program and build a map for locating them.
	pub fn find(program: &Program<'ast>, compiler: &mut Compiler) -> Result<Names<'ast>, CompileError> {
		let mut names = Names {
			parameters: HashMap::new(),
			procedures: HashMap::new(),
			variables: vec![HashMap::new(); program.procedures.len()],
			combinators: HashMap::new(),
		};

		// Insert built-in functions and modules
		for (name, sig, code) in BUILTIN_FUNCTIONS {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Function,
				definition: ProcedureDefinition::BuiltIn { sig, code },
			});
		}
		for (name, sig) in BUILTIN_MODULES {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Module,
				definition: ProcedureDefinition::BuiltIn { sig, code: &[] },
			});
		}
		for proc in PRECOMPILED_FUNCTIONS {
			names.procedures.insert(proc.0, ProcedureRef {
				kind: ProcedureKind::Function,
				definition: ProcedureDefinition::Precompiled { proc },
			});
		}
		for proc in PRECOMPILED_MODULES {
			names.procedures.insert(proc.0, ProcedureRef {
				kind: ProcedureKind::Module,
				definition: ProcedureDefinition::Precompiled { proc },
			});
		}
		for &(name, neutral, code) in REPETITION_COMBINATORS {
			names.combinators.insert(name, Combinator { neutral, code });
		}

		// Insert parameters
		for (index, param) in program.parameters.iter().enumerate() {
			names.parameters.insert(param.name.text, VariableRef{
				kind: VariableKind::Parameter,
				tuple_index: index,
			});
		}

		// Run through all procedures in the program
		for (proc_index, proc) in program.procedures.iter().enumerate() {
			let proc_ref = ProcedureRef {
				kind: proc.kind,
				definition: ProcedureDefinition::Declaration { proc_index },
			};
			names.insert_procedure(program, compiler, &proc.name, proc_ref);

			// Run through all patterns in the procedure; first the inputs,
			// then the left-hand sides of all assignments.
			for (tuple_index, item) in proc.inputs.items.iter().enumerate() {
				let var_ref = VariableRef {
					kind: VariableKind::Input,
					tuple_index,
				};
				names.insert_variable(program, compiler, proc_index, &item.name, var_ref);
			}
			for (body_index, Statement::Assign { ref node, exp }) in proc.body.iter().enumerate() {
				for (tuple_index, item) in node.items.iter().enumerate() {
					let var_ref = VariableRef {
						kind: VariableKind::Node { body_index },
						tuple_index,
					};
					names.insert_variable(program, compiler, proc_index, &item.name, var_ref);
				}
				// Add any repetition variables in the right-hand side.
				exp.traverse_pre(&mut |exp| {
					if let Expression::For { name, .. } = exp {
						let var_ref = VariableRef {
							kind: VariableKind::For { variable_pos: PosRange::from(name) },
							tuple_index: 0,
						};
						names.insert_variable(program, compiler, proc_index, &name, var_ref);
					}
				});
			}
		}

		compiler.if_no_errors(names)
	}

	fn insert_procedure(&mut self,
			program: &Program<'ast>, compiler: &mut Compiler,
			name: &Id<'ast>, proc_ref: ProcedureRef) {
		self.procedures.entry(name.text).and_modify(|existing| {
			match existing.definition {
				ProcedureDefinition::BuiltIn { .. } | ProcedureDefinition::Precompiled { .. } => {
					compiler.report_error(name,
						format!("The {} '{}' has the same name as a built-in {}.",
							proc_ref.kind, name, existing.kind));
				},
				ProcedureDefinition::Declaration { proc_index } => {
					compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
					let existing_name = &program.procedures[proc_index].name;
					compiler.report_context(existing_name, "Previously defined here.");
				},
			}
		}).or_insert(proc_ref);
	}

	fn insert_variable(&mut self,
			program: &Program<'ast>, compiler: &mut Compiler,
			proc_index: usize, name: &Id<'ast>, var_ref: VariableRef) {
		if name.text == "_" { return; }
		self.variables[proc_index].entry(name.text).and_modify(|existing| {
			compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
			let proc = &program.procedures[proc_index];
			let loc: &dyn Location = match existing.kind {
				VariableKind::Parameter => &program.parameters[existing.tuple_index].name,
				VariableKind::Input => &proc.inputs.items[existing.tuple_index].name,
				VariableKind::Node { body_index } => match proc.body[body_index] {
					Statement::Assign { ref node, .. } => &node.items[existing.tuple_index].name,
				},
				VariableKind::For { ref variable_pos } => variable_pos,
			};
			compiler.report_context(loc, "Previously defined here.");
		}).or_insert(var_ref);
	}

	/// Look up a module, function or instrument by name.
	pub fn lookup_procedure(&self, name: &str) -> Option<&ProcedureRef> {
		self.procedures.get(name)
	}

	/// Look up a variable by name inside a specific procedure.
	pub fn lookup_variable(&self, proc_index: usize, name: &str) -> Option<&VariableRef> {
		self.variables[proc_index].get(name).or_else(|| self.parameters.get(name))
	}

	pub fn lookup_combinator(&self, combinator: &str) -> Option<&Combinator> {
		self.combinators.get(combinator)
	}

	pub fn combinator_list(&self) -> String {
		let mut combinators: Vec<&'ast str> = self.combinators.keys().cloned().collect();
		combinators.sort();
		let mut list = format!("'{}'", combinators[0]);
		for i in 1 .. combinators.len() - 1 {
			list += &format!(", '{}'", combinators[i]);
		}
		list += &format!(" and '{}'", combinators.last().unwrap());
		list
	}

	pub fn lookup_parameter(&self, name: &str) -> Option<usize> {
		self.parameters.get(name).map(|p| p.tuple_index)
	}

	pub fn parameter_names(&self) -> impl Iterator<Item=&&'ast str> {
		self.parameters.keys()
	}
}
