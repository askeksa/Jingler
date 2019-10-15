#![allow(unused)]

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
