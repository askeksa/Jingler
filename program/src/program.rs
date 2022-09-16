use std::fmt::{Display, Error, Formatter};

use crate::instructions::Instruction;

#[derive(Clone, Debug)]
pub struct ZingProgram {
	// Procedures
	pub procedures: Vec<ZingProcedure>,
	// Instruments in execution order
	pub instrument_order: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct ZingProcedure {
	pub name: String,
	pub kind: ZingProcedureKind,
	pub code: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ZingProcedureKind {
	Function,
	Module { scope: ZingScope },
	Instrument { scope: ZingScope },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ZingScope {
	Static,
	Dynamic,
}

impl Display for ZingProcedureKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ZingProcedureKind::Function => write!(f, "function"),
			ZingProcedureKind::Module { scope } => write!(f, "module, {} part", scope),
			ZingProcedureKind::Instrument { scope } => write!(f, "instrument, {} part", scope),
		}
	}
}

impl Display for ZingScope {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ZingScope::Static => write!(f, "static"),
			ZingScope::Dynamic => write!(f, "dynamic"),
		}
	}
}
