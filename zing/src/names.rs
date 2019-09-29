
use std::collections::HashMap;
use std::iter::once;

use crate::ast::*;
use crate::builtin::{BUILTIN_FUNCTIONS, BUILTIN_MODULES};
use crate::compiler::{Compiler, CompileError};

#[derive(Clone, Debug)]
pub struct Names<'input> {
	procedures: HashMap<&'input str, ProcedureRef>,
	variables: Vec<HashMap<&'input str, VariableRef>>,
}

#[derive(Clone, Debug)]
pub struct VariableRef {
	pub kind: VariableKind,
	pub tuple_index: usize,
	pub channel: ChannelKind,
}

#[derive(Clone, Copy, Debug)]
pub enum VariableKind {
	Input,
	Node { index: usize },
}

#[derive(Clone, Copy, Debug)]
pub enum ChannelKind {
	Single,
	Side { index: usize }
}

#[derive(Clone, Debug)]
pub struct ProcedureRef {
	pub kind: ProcedureKind,
	pub definition: ProcedureDefinition,
}

#[derive(Clone, Debug)]
pub enum ProcedureDefinition {
	BuiltIn { signature: &'static Signature<'static> },
	Declaration { index: usize },
}

#[derive(Clone, Debug)]
pub struct Signature<'sig> {
	pub params: &'sig [Type],
	pub outputs: &'sig [Type],
}

impl<'input> Names<'input> {
	pub fn find(program: &Program<'input>, compiler: &mut Compiler) -> Result<Names<'input>, CompileError> {
		let mut names = Names {
			procedures: HashMap::new(),
			variables: vec![HashMap::new(); program.declarations.len()],
		};

		for (name, sig) in BUILTIN_FUNCTIONS {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Function,
				definition: ProcedureDefinition::BuiltIn { signature: sig },
			});
		}
		for (name, sig) in BUILTIN_MODULES {
			names.procedures.insert(name, ProcedureRef {
				kind: ProcedureKind::Module,
				definition: ProcedureDefinition::BuiltIn { signature: sig },
			});
		}

		for (index, decl) in program.declarations.iter().enumerate() {
			let Declaration::Procedure { kind, name, params, body, .. } = decl;
			let proc_ref = ProcedureRef {
				kind: *kind,
				definition: ProcedureDefinition::Declaration { index },
			};
			names.insert_procedure(program, compiler, name, proc_ref);

			let input = (VariableKind::Input, params);
			let nodes = (0..body.len()).map(|index| {
				let Statement::Assign { ref node, .. } = body[index];
				(VariableKind::Node { index }, node)
			});
			for (variable_kind, node) in once(input).chain(nodes) {
				for (tuple_index, item) in node.iter().enumerate() {
					match &item.variable {
						PatternVariable::Variable { name } => {
							let var_ref = VariableRef {
								kind: variable_kind,
								tuple_index,
								channel: ChannelKind::Single,
							};
							names.insert_variable(program, compiler, index, name, var_ref);
						},
						PatternVariable::Split { left, right } => {
							for (side_index, side_name) in [left, right].iter().enumerate() {
								let var_ref = VariableRef {
									kind: variable_kind,
									tuple_index,
									channel: ChannelKind::Side { index: side_index },
								};
								names.insert_variable(program, compiler, index, side_name, var_ref);
							}
						},
					}
				}
			}
		}

		compiler.if_not_error(names)
	}

	fn insert_procedure(&mut self, program: &Program<'input>, compiler: &mut Compiler,
			name: &Id<'input>, proc_ref: ProcedureRef) {
		self.procedures.entry(name.text).and_modify(|existing| {
			match existing.definition {
				ProcedureDefinition::BuiltIn { .. } => {
					compiler.report_error(name,
						format!("The {} '{}' has the same name as a built-in {}.",
							proc_ref.kind, name, existing.kind));
				},
				ProcedureDefinition::Declaration { index } => {
					compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
					match program.declarations[index] {
						Declaration::Procedure { name: ref existing_name, .. } => {
							compiler.report_context(existing_name, "Previously defined here.");
						},
					}
				},
			}
		}).or_insert(proc_ref);
	}

	fn insert_variable(&mut self, program: &Program<'input>, compiler: &mut Compiler,
			decl_index: usize, name: &Id<'input>, var_ref: VariableRef) {
		self.variables[decl_index].entry(name.text).and_modify(|existing| {
			compiler.report_error(name, format!("Duplicate definition of '{}'.", name));
			let pattern = match &program.declarations[decl_index] {
				Declaration::Procedure { params, body, .. } => {
					match existing.kind {
						VariableKind::Input => params,
						VariableKind::Node { index } => match body[index] {
							Statement::Assign { ref node, .. } => node,
						},
					}
				},
			};
			let existing_name = match (&pattern[existing.tuple_index].variable, existing.channel) {
				(PatternVariable::Variable { name }, ChannelKind::Single) => name,
				(PatternVariable::Split { left, right }, ChannelKind::Side { index }) => {
					[left, right][index]
				},
				_ => panic!("Mismatch between PatternVariable and ChannelKind"),
			};
			compiler.report_context(existing_name, "Previously defined here.");
		}).or_insert(var_ref);
	}

	pub fn lookup_procedure(&self, name: &str) -> Option<&ProcedureRef> {
		self.procedures.get(name)
	}

	pub fn lookup_variable(&self, decl_index: usize, name: &str) -> Option<&VariableRef> {
		self.variables[decl_index].get(name)
	}
}
