use std::fmt::{Display, Error, Formatter};

use crate::instructions::Instruction;

#[derive(Clone, Debug)]
pub struct ZingProgram {
	// Parameters
	pub parameters: Vec<ZingParameter>,
	// Procedures
	pub procedures: Vec<ZingProcedure>,
	// Instruments in execution order
	pub instrument_order: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct ZingParameter {
	pub name: String,
	pub min: f32,
	pub max: f32,
	pub default: f32,
}

#[derive(Clone, Debug)]
pub struct ZingProcedure {
	pub name: String,
	pub kind: ZingProcedureKind,
	pub inputs: Vec<ZingType>,
	pub outputs: Vec<ZingType>,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ZingWidth {
	Mono,
	Stereo,
	Generic,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ZingValueType {
	Number,
	Buffer,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ZingType {
	pub width: ZingWidth,
	pub value_type: ZingValueType,
}

impl Display for ZingProcedure {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "{} [{}]: ", self.name, self.kind)?;
		write_list(f, &self.inputs)?;
		write!(f, " -> ")?;
		write_list(f, &self.outputs)
	}
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

impl Display for ZingWidth {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ZingWidth::Mono => write!(f, "mono"),
			ZingWidth::Stereo => write!(f, "stereo"),
			ZingWidth::Generic => write!(f, "generic"),
		}
	}
}

impl Display for ZingValueType {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ZingValueType::Number => write!(f, "number"),
			ZingValueType::Buffer => write!(f, "buffer"),
		}
	}
}

impl Display for ZingType {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "{} {}", self.width, self.value_type)
	}
}

fn write_list(f: &mut Formatter, list: &Vec<impl Display>) -> Result<(), Error> {
	write!(f, "(")?;
	let mut first = true;
	for item in list {
		if !first {
			write!(f, ", ")?;
		}
		write!(f, "{}", item)?;
		first = false;
	}
	write!(f, ")")
}
