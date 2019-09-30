
use std::iter::repeat;

use crate::ast::*;
use crate::compiler::{CompileError, Compiler};
use crate::names::Names;

pub fn infer_types<'ast, 'input, 'comp>(
		program: &'comp mut Program<'ast>,
		names: &'ast Names<'ast>,
		compiler: &'comp mut Compiler<'input>) -> Result<(), CompileError> {
	let mut type_inferrer = TypeInferrer::new(names, compiler);
	type_inferrer.infer_signatures(program);
	compiler.check_errors()
}


struct TypeInferrer<'ast, 'input, 'comp> {
	names: &'ast Names<'ast>,
	compiler: &'comp mut Compiler<'input>,
}

impl<'ast, 'input, 'comp> TypeInferrer<'ast, 'input, 'comp> {
	pub fn new(names: &'ast Names<'ast>, compiler: &'comp mut Compiler<'input>)
			-> TypeInferrer<'ast, 'input, 'comp> {
		TypeInferrer {
			names,
			compiler,
		}
	}

	pub fn infer_signatures(&mut self, program: &'comp mut Program<'ast>) {
		for decl in &mut program.declarations {
			let Declaration::Procedure { kind, name, params, outputs, .. } = decl;
			let mut instrument_types: Vec<Type> = vec![];
			let mut output_count = 0;
			for (is_output, item) in repeat(false).zip(params).chain(repeat(true).zip(outputs)) {
				let PatternItem { variable, item_type } = item;
				match kind {
					// Module inputs/outputs default to dynamic stereo number, and only
					// inputs can be static.
					ProcedureKind::Module => {
						item_type.scope = item_type.scope.or(Some(Scope::Dynamic));
						if is_output {
							if let Some(Scope::Static) = item_type.scope {
								self.compiler.report_error(&*variable,
									"Module outputs can't be static.");
							}
						}
						item_type.width = item_type.width.or(Some(Width::Stereo));
						item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
					},
					// Function inputs/outputs default to stereo number and have no
					// static/dynamic distinction.
					ProcedureKind::Function => {
						if item_type.scope.is_some() {
							self.compiler.report_error(&*variable,
								"Function inputs or outputs can't be marked static or dynamic.");
						}
						item_type.width = item_type.width.or(Some(Width::Stereo));
						item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
					},
					// Instrument inputs default to dynamic stereo number, only
					// inputs can be static, all static inputs must appear before all
					// dynamic inputs, there must be the same number of dynamic inputs
					// and outputs, the types of these must match up, and outputs
					// default to the types of their corresponding inputs.
					ProcedureKind::Instrument => {
						item_type.scope = item_type.scope.or(Some(Scope::Dynamic));
						if is_output {
							match item_type.scope.unwrap() {
								Scope::Static => {
									self.compiler.report_error(&*variable,
										"Instrument outputs can't be static.");
								},
								Scope::Dynamic => {
									if output_count < instrument_types.len() {
										let input = &instrument_types[output_count];
										item_type.width = item_type.width.or(input.width);
										item_type.value_type = item_type.value_type.or(input.value_type);
										if item_type != input {
											self.compiler.report_error(&*variable,
												"Instrument output type doesn't match the corresponding input.");
										}
									} else if output_count == instrument_types.len() {
										self.compiler.report_error(&*variable,
											"Instrument has more outputs than dynamic inputs.");
									}
									output_count += 1;
								},
							}
						} else {
							item_type.width = item_type.width.or(Some(Width::Stereo));
							item_type.value_type = item_type.value_type.or(Some(ValueType::Number));
							match item_type.scope.unwrap() {
								Scope::Static => {
									if !instrument_types.is_empty() {
										self.compiler.report_error(&*variable,
											"Static instrument inputs can't come after dynamic inputs.");
									}
								},
								Scope::Dynamic => {
									instrument_types.push(*item_type);
								},
							}
						}
					},
				}
				// A split pattern can only be used for stereo values, and buffer
				// values can't be split.
				if let PatternVariable::Split { .. } = variable {
					if let Some(Width::Stereo) = item_type.width {
						// OK
					} else {
						self.compiler.report_error(&*variable, "Only stereo values can be split.");
					}
					if let Some(ValueType::Buffer) = item.item_type.value_type {
						self.compiler.report_error(&*variable, "Buffer values can't be split.");
					}
				}
			}
			if output_count < instrument_types.len() {
				self.compiler.report_error(&*name, "Instrument has fewer outputs than dynamic inputs.");
			}
		}
	}
}
