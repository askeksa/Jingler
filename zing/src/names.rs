
use std::collections::HashMap;
use std::iter::once;

use crate::ast::*;
use crate::bytecodes::*;
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
	Declaration { decl_index: usize },
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
			variables: vec![HashMap::new(); program.declarations.len()],
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

		// Run through all declarattions in the program
		for (decl_index, decl) in program.declarations.iter().enumerate() {
			let Declaration::Procedure { kind, name: proc_name, inputs, body, .. } = decl;
			let proc_ref = ProcedureRef {
				kind: *kind,
				definition: ProcedureDefinition::Declaration { decl_index },
			};
			names.insert_procedure(program, compiler, proc_name, proc_ref);

			// Run through all patterns in the procedure; first the inputs,
			// then the left-hand sides of all assignments.
			let input = (VariableKind::Input, inputs);
			let nodes = (0..body.len()).map(|body_index| {
				let Statement::Assign { ref node, .. } = body[body_index];
				(VariableKind::Node { body_index }, node)
			});
			for (variable_kind, node) in once(input).chain(nodes) {
				for (tuple_index, item) in node.items.iter().enumerate() {
					let var_ref = VariableRef {
						kind: variable_kind,
						tuple_index,
					};
					names.insert_variable(program, compiler, decl_index, &item.name, var_ref);
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
				ProcedureDefinition::Declaration { decl_index } => {
					compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
					match program.declarations[decl_index] {
						Declaration::Procedure { name: ref existing_name, .. } => {
							compiler.report_context(existing_name, "Previously defined here.");
						},
					}
				},
			}
		}).or_insert(proc_ref);
	}

	fn insert_variable(&mut self,
			program: &Program<'ast>, compiler: &mut Compiler,
			decl_index: usize, name: &Id<'ast>, var_ref: VariableRef) {
		self.variables[decl_index].entry(name.text).and_modify(|existing| {
			compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
			let pattern = match &program.declarations[decl_index] {
				Declaration::Procedure { inputs, body, .. } => {
					match existing.kind {
						VariableKind::Input => inputs,
						VariableKind::Node { body_index } => match body[body_index] {
							Statement::Assign { ref node, .. } => node,
						},
					}
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

	/// Look up a variable by name inside a specific declaration.
	pub fn lookup_variable(&self, decl_index: usize, name: &str) -> Option<&VariableRef> {
		self.variables[decl_index].get(name)
	}
}
