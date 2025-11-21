use std::fmt::{Display, Error, Formatter};

use crate::instructions::Instruction;

#[derive(Clone, Debug)]
pub struct Program {
	// Parameters
	pub parameters: Vec<Parameter>,
	// Procedures
	pub procedures: Vec<Procedure>,
	// Static procedure ID of the main module
	pub main_static_proc_id: usize,
	// Dynamic procedure ID of the main module
	pub main_dynamic_proc_id: usize,
	// Tracks in execution order
	pub track_order: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct Parameter {
	pub name: String,
	pub min: f32,
	pub max: f32,
	pub default: f32,
}

#[derive(Clone, Debug)]
pub struct Procedure {
	pub name: String,
	pub kind: ProcedureKind,
	pub inputs: Vec<Type>,
	pub outputs: Vec<Type>,
	pub code: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProcedureKind {
	Function,
	Module { scope: Scope },
	Instrument { scope: Scope },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Scope {
	Static,
	Dynamic,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Width {
	Mono,
	Stereo,
	Generic,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ValueType {
	Number,
	Buffer,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Type {
	pub width: Width,
	pub value_type: ValueType,
}

impl Display for Procedure {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "{} [{}]: ", self.name, self.kind)?;
		write_list(f, &self.inputs)?;
		write!(f, " -> ")?;
		write_list(f, &self.outputs)
	}
}

impl Display for ProcedureKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ProcedureKind::Function => write!(f, "function"),
			ProcedureKind::Module { scope } => write!(f, "module, {} part", scope),
			ProcedureKind::Instrument { scope } => write!(f, "instrument, {} part", scope),
		}
	}
}

impl Display for Scope {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			Scope::Static => write!(f, "static"),
			Scope::Dynamic => write!(f, "dynamic"),
		}
	}
}

impl Display for Width {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			Width::Mono => write!(f, "mono"),
			Width::Stereo => write!(f, "stereo"),
			Width::Generic => write!(f, "generic"),
		}
	}
}

impl Display for ValueType {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			ValueType::Number => write!(f, "number"),
			ValueType::Buffer => write!(f, "buffer"),
		}
	}
}

impl Display for Type {
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
