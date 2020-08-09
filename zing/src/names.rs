
use std::collections::HashMap;
use std::iter::once;

use bytecode::bytecodes::Bytecode;

use crate::ast::*;
use crate::builtin::{BUILTIN_FUNCTIONS, BUILTIN_MODULES};
use crate::compiler::{Compiler, CompileError};

/// Mapping of names in the program to their definitions.
#[derive(Clone, Debug)]
pub struct Names<'ast> {
	procedures: HashMap<&'ast str, ProcedureRef>,
	variables: Vec<HashMap<&'ast str, VariableRef>>,
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
	Input,
	Node { body_index: usize },
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
		bc: &'static [Bytecode],
	},
	Declaration { proc_index: usize },
}

/// Signature of a procedure or operator.
#[derive(Clone, Debug)]
pub struct Signature<'sig> {
	pub inputs: &'sig [Type],
	pub outputs: &'sig [Type],
}

impl<'ast> Names<'ast> {
	/// Find all named entities in the program and build a map for locating them.
	pub fn find(program: &Program<'ast>, compiler: &mut Compiler) -> Result<Names<'ast>, CompileError> {
		let mut names = Names {
			procedures: HashMap::new(),
			variables: vec![HashMap::new(); program.procedures.len()],
		};

		// Insert built-in functions and modules
		for (name, sig, bc) in BUILTIN_FUNCTIONS {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Function,
				definition: ProcedureDefinition::BuiltIn { sig, bc },
			});
		}
		for (name, sig) in BUILTIN_MODULES {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Module,
				definition: ProcedureDefinition::BuiltIn { sig, bc: &[] },
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
			let input = (VariableKind::Input, &proc.inputs);
			let nodes = (0..proc.body.len()).map(|body_index| {
				let Statement::Assign { ref node, .. } = proc.body[body_index];
				(VariableKind::Node { body_index }, node)
			});
			for (variable_kind, node) in once(input).chain(nodes) {
				for (tuple_index, item) in node.items.iter().enumerate() {
					let var_ref = VariableRef {
						kind: variable_kind,
						tuple_index,
					};
					names.insert_variable(program, compiler, proc_index, &item.name, var_ref);
				}
			}
		}

		compiler.if_no_errors(names)
	}

	fn insert_procedure(&mut self,
			program: &Program<'ast>, compiler: &mut Compiler,
			name: &Id<'ast>, proc_ref: ProcedureRef) {
		self.procedures.entry(name.text).and_modify(|existing| {
			match existing.definition {
				ProcedureDefinition::BuiltIn { .. } => {
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
			let pattern = match existing.kind {
				VariableKind::Input => &proc.inputs,
				VariableKind::Node { body_index } => match proc.body[body_index] {
					Statement::Assign { ref node, .. } => node,
				},
			};
			let existing_name = &pattern.items[existing.tuple_index].name;
			compiler.report_context(existing_name, "Previously defined here.");
		}).or_insert(var_ref);
	}

	/// Look up a module, function or instrument by name.
	pub fn lookup_procedure(&self, name: &str) -> Option<&ProcedureRef> {
		self.procedures.get(name)
	}

	/// Look up a variable by name inside a specific procedure.
	pub fn lookup_variable(&self, proc_index: usize, name: &str) -> Option<&VariableRef> {
		self.variables[proc_index].get(name)
	}
}
