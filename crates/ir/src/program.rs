use std::fmt::{Display, Error, Formatter};

use serde::{Deserialize, Serialize};

use crate::instructions::Instruction;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Program {
	// Parameters
	pub parameters: Vec<Parameter>,
	// Procedures
	pub procedures: Vec<Procedure>,
	// Static procedure ID of the main module
	pub main_static_proc_id: usize,
	// Dynamic procedure ID of the main module
	pub main_dynamic_proc_id: usize,
	// MIDI mapping (zero based channel, note range, transposition) for each
	// track in execution order
	pub track_order: Vec<MidiMapping>,
}

impl Program {
	/// Name of the instrument procedure played by each `PlayInstrument`
	/// instruction, in execution order.
	///
	/// The runtime assigns track indices in the order the `PlayInstrument`
	/// instructions execute, so entry `i` of the returned vector describes the
	/// same track as `track_order[i]`.
	pub fn track_procedure_names(&self) -> Vec<&str> {
		let mut names = vec![];
		let mut on_stack = vec![false; self.procedures.len()];
		self.collect_track_procedure_names(self.main_dynamic_proc_id, &mut on_stack, &mut names);
		names
	}

	fn collect_track_procedure_names<'a>(&'a self, proc_id: usize,
			on_stack: &mut Vec<bool>, names: &mut Vec<&'a str>) {
		let Some(procedure) = self.procedures.get(proc_id) else { return };
		if on_stack[proc_id] {
			// Modules cannot be recursive, but never loop forever on a malformed program.
			return;
		}
		on_stack[proc_id] = true;
		for inst in &procedure.code {
			match *inst {
				Instruction::PlayInstrument(_, dynamic_proc) => {
					if let Some(played) = self.procedures.get(dynamic_proc as usize) {
						names.push(&played.name);
					}
				},
				Instruction::Call(callee, _) => {
					// Only dynamic module procedures can reach a PlayInstrument.
					let callee = callee as usize;
					if let Some(p) = self.procedures.get(callee)
							&& p.kind == (ProcedureKind::Module { scope: Scope::Dynamic }) {
						self.collect_track_procedure_names(callee, on_stack, names);
					}
				},
				_ => {},
			}
		}
		on_stack[proc_id] = false;
	}
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct MidiMapping {
	// Zero based MIDI channel (0..=15)
	pub channel: u8,
	// Inclusive note range start (0..=127)
	pub start: u8,
	// Inclusive note range end (0..=127)
	pub end: u8,
	// Key that the range start is mapped to (0..=127)
	pub transpose_to: u8,
}

impl MidiMapping {
	/// If a `key` arriving on MIDI `channel` triggers this track, return the
	/// (possibly transposed) key the instrument should receive; otherwise `None`.
	pub fn triggered_key(&self, channel: u8, key: u8) -> Option<u8> {
		if channel == self.channel && key >= self.start && key <= self.end {
			// key >= start, and start/end/transpose_to are all <= 127, so the
			// result lies in 0..=254 — fits in u8 with no under/overflow.
			Some(key - self.start + self.transpose_to)
		} else {
			None
		}
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Parameter {
	pub name: String,
	pub min: f32,
	pub max: f32,
	pub default: f32,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Procedure {
	pub name: String,
	pub kind: ProcedureKind,
	pub inputs: Vec<Type>,
	pub outputs: Vec<Type>,
	pub code: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ProcedureKind {
	Function,
	Module { scope: Scope },
	Instrument { scope: Scope },
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Scope {
	Static,
	Dynamic,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Width {
	Mono,
	Stereo,
	Generic,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum ValueType {
	Number,
	Buffer,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
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

#[cfg(test)]
mod tests {
	use super::*;
	use crate::instructions::Instruction::*;

	fn procedure(name: &str, kind: ProcedureKind, code: Vec<crate::instructions::Instruction>) -> Procedure {
		Procedure { name: name.to_string(), kind, inputs: vec![], outputs: vec![], code }
	}

	fn module(name: &str, code: Vec<crate::instructions::Instruction>) -> Procedure {
		procedure(name, ProcedureKind::Module { scope: Scope::Dynamic }, code)
	}

	fn instrument(name: &str) -> Procedure {
		procedure(name, ProcedureKind::Instrument { scope: Scope::Dynamic }, vec![])
	}

	#[test]
	fn track_procedure_names_follow_the_play_instrument_order() {
		//  0: main (dynamic), 1: sub (dynamic), 2/3: bass, 4/5: lead, 6: a function
		let program = Program {
			parameters: vec![],
			procedures: vec![
				module("main", vec![PlayInstrument(2, 3), Call(6, None), Call(1, None), PlayInstrument(4, 5)]),
				module("sub", vec![PlayInstrument(4, 5), PlayInstrument(2, 3)]),
				procedure("bass", ProcedureKind::Instrument { scope: Scope::Static }, vec![]),
				instrument("bass"),
				procedure("lead", ProcedureKind::Instrument { scope: Scope::Static }, vec![]),
				instrument("lead"),
				procedure("helper", ProcedureKind::Function, vec![PlayInstrument(2, 3)]),
			],
			main_static_proc_id: 0,
			main_dynamic_proc_id: 0,
			track_order: vec![],
		};

		// Depth first through the module calls, and not into the function.
		assert_eq!(program.track_procedure_names(), vec!["bass", "lead", "bass", "lead"]);
	}

	#[test]
	fn track_procedure_names_terminates_on_a_recursive_program() {
		let program = Program {
			parameters: vec![],
			procedures: vec![
				module("main", vec![Call(1, None)]),
				module("sub", vec![PlayInstrument(2, 2), Call(0, None)]),
				instrument("beep"),
			],
			main_static_proc_id: 0,
			main_dynamic_proc_id: 0,
			track_order: vec![],
		};

		assert_eq!(program.track_procedure_names(), vec!["beep"]);
	}
}
