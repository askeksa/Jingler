
use std::f32;
use std::fmt::{Display, Error, Formatter};

#[macro_export]
macro_rules! bc {
	{ $($b:expr),* } => {
		{
			#[allow(unused)] use Bytecode::*;
			#[allow(unused)] use NoteProperty::*;
			&[$($b),*]
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Bytecode {
	// Constants
	Constant(u32),
	SampleRate,

	// Binary operations
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

	// Comparisons
	Eq,
	Greater,
	GreaterEq,
	Less,
	LessEq,
	Neq,

	// Rounding
	Ceil,
	Floor,
	Round,
	Trunc,

	// Math
	Atan2,
	Cos,
	Exp2,
	Mlog2,
	Sin,
	Sqrt,
	Tan,

	// Channels
	Left,
	Right,
	ExpandStereo,
	ExpandGeneric,
	SplitRL,
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
	BufferLoadWithOffset,
	BufferStoreAndStep,
	BufferIndex,
	BufferLength,

	// Procedures/instruments/notes
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
	GmDlsLength,
	GmDlsSample,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NoteProperty {
	Gate,
	Key,
	Velocity,
}

impl Bytecode {
	pub fn stack_change(&self) -> (usize, usize) { // pops, pushes
		use Bytecode::*;
		match self {
			Constant(..) | SampleRate => (0, 1),

			Add | Sub | AddSub | Mul | Div => (2, 1),
			And | AndNot | Or | Xor => (2, 1),
			Min | Max => (2, 1),

			Eq | Greater | GreaterEq | Less | LessEq | Neq => (2, 1),
			Ceil | Floor | Round | Trunc => (1, 1),

			Atan2 | Mlog2 => (2, 1),
			Cos | Exp2 | Sin | Sqrt | Tan => (1, 1),

			Left | Right | ExpandStereo | ExpandGeneric => (1, 1),
			SplitRL => (1, 2),
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
			BufferLoad => (1, 1),
			BufferLoadWithOffset => (2, 1),
			BufferStoreAndStep => (2, 1),
			BufferIndex | BufferLength => (1, 1),

			Call(..) => panic!("stack_change on 'call'"),
			CallInstrument => (0, 0),
			Kill => (1, 0),
			ReadNoteProperty(..) => (0, 1),

			Cmp => (2, 2),
			Label | Loop | If | EndIf => (0, 0),
			Else => panic!("stack_change on 'else'"),

			Random => (2, 1),
			GmDlsLength => (1, 1),
			GmDlsSample => (2, 1),
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

pub trait HasBytecodes {
	fn get_bytecodes(&self) -> &[Bytecode];
}

impl HasBytecodes for [Bytecode] {
	fn get_bytecodes(&self) -> &[Bytecode] {
		&self
	}
}

impl HasBytecodes for &[Bytecode] {
	fn get_bytecodes(&self) -> &[Bytecode] {
		self
	}
}

impl HasBytecodes for Vec<Bytecode> {
	fn get_bytecodes(&self) -> &[Bytecode] {
		&self[..]
	}
}
