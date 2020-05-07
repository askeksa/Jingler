#![allow(unused)]

use std::f32;
use std::fmt::{Display, Error, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Bytecode {
	// Constants
	Constant(u32),

	// Operations
	Add,
	Sub,
	AddSub,
	Mul,
	Div,
	And,
	AndNot,
	Or,
	Xor,
	Min,
	Max,
	Sqrt,
	Round(RoundingMode),
	Compare(CompareOp),

	// FPU stack ops
	Fputnext,
	Fop(Fopcode),
	Exp2Body,
	Fdone,

	// Channels
	Expand,
	SplitRL,
	SplitLR,
	MergeLR,

	// Stack manipulation
	Pop,
	PopNext,
	StackLoad(u16),
	StackStore(u16),

	// State
	CellInit,
	CellRead,
	CellStore(u16),
	StateEnter,
	StateLeave,

	// Buffers
	BufferAlloc,
	BufferLoad,
	BufferStore,
	BufferLength,

	// Procedures/instruments/notes
	Proc,
	Call(u16),
	CallInstrument,
	Kill,
	ReadNoteProperty(NoteProperty),

	// Loops and conditions
	Cmp,
	Label,
	Loop,
	If,
	Else,
	EndIf,

	// Misc
	Random,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RoundingMode {
	Nearest = 0,
	Floor = 1,
	Ceil = 2,
	Truncate = 3,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompareOp {
	Eq = 0,
	Less = 1,
	LessEq = 2,
	Neq = 4,
	GreaterEq = 5,
	Greater = 6,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NoteProperty {
	Length = 0,
	Tone = 1,
	Velocity = 2,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Fopcode {
	Fyl2x = 0xF1,
	Fptan = 0xF2,
	Fpatan = 0xF3,
	Frndint = 0xFC,
	Fsin = 0xFE,
	Fcos = 0xFF,
}

impl Bytecode {
	pub fn stack_change(&self) -> (usize, usize) { // pops, pushes
		use Bytecode::*;
		match self {
			Constant(..) => (0, 1),

			Add | Sub | AddSub | Mul | Div => (2, 1),
			And | AndNot | Or | Xor => (2, 1),
			Min | Max | Compare(..) => (2, 1),
			Sqrt | Round(..) => (1, 1),

			Fputnext => (2, 1),
			Fop(..) | Exp2Body | Fdone => (1, 1),

			Expand => (1, 1),
			SplitRL | SplitLR => (1, 2),
			MergeLR => (2, 1),

			Pop => (1, 0),
			PopNext => (2, 1),
			StackLoad(offset) => {
				let offset = *offset as usize;
				(offset + 1, offset + 2)
			},
			StackStore(offset) => {
				let offset = *offset as usize;
				(offset + 2, offset + 1)
			},

			CellInit => (1, 0),
			CellRead => (0, 1),
			CellStore(..) => (1, 0),
			StateEnter | StateLeave => (0, 0),

			BufferAlloc => (1, 1),
			BufferLoad => (2, 1),
			BufferStore => (3, 0),
			BufferLength => (1, 1),

			Proc => panic!("stack_change on 'proc'"),
			Call(..) => panic!("stack_change on 'call'"),
			CallInstrument => (0, 0),
			Kill => (1, 0),
			ReadNoteProperty(..) => (0, 1),

			Cmp => (2, 2),
			Label | Loop | If | EndIf => (0, 0),
			Else => panic!("stack_change on 'else'"),

			Random => (2, 1),
		}
	}
}

impl Display for Bytecode {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match *self {
			Bytecode::Constant(c) => {
				write!(f, "Constant({})", f32::from_bits(c))?;
			},
			bc => {
				write!(f, "{:?}", bc)?;
			},
		}
		Ok(())
	}
}
