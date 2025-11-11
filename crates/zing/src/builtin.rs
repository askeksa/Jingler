
use program::instructions::*;

use crate::ast::{BinOp, BinOpKind, Context, Scope, Type, UnOp, UnOpKind, ValueType, Width};
use crate::names::Signature;

macro_rules! scope {
	{ static } => { Scope::Static };
	{ dynamic } => { Scope::Dynamic };
}

macro_rules! width {
	{ mono } => { Width::Mono };
	{ stereo } => { Width::Stereo };
	{ generic } => { Width::Generic };
}

macro_rules! value_type {
	{ number } => { ValueType::Number };
	{ bool } => { ValueType::Bool };
	{ buffer } => { ValueType::Buffer };
	{ typeless } => { ValueType::Typeless };
}

#[macro_export]
macro_rules! type_spec {
	{ $w:ident } => {
		Type {
			scope: None,
			width: Some(width!($w)),
			value_type: Some(ValueType::Number),
		}
	};
	{ $w:ident $t:ident } => {
		Type {
			scope: None,
			width: Some(width!($w)),
			value_type: Some(value_type!($t)),
		}
	};
	{ $s:ident $w:ident $t:ident } => {
		Type {
			scope: Some(scope!($s)),
			width: Some(width!($w)),
			value_type: Some(value_type!($t)),
		}
	};
}

#[macro_export]
macro_rules! sig {
	{ [ $($($p:ident)+),* ] [ $($($o:ident)+),* ] } => {
		Signature {
			inputs: &[$(type_spec!($($p)+)),*],
			outputs: &[$(type_spec!($($o)+)),*],
		}
	};
}

pub type BuiltinFunction = (&'static str, Context, Signature<'static>, &'static [Instruction]);
pub type BuiltinModule = (&'static str, Context, Signature<'static>);
pub type PrecompiledProcedure = (&'static str, Context, Signature<'static>, &'static [&'static [Instruction]]);

pub trait PrecompiledProcedureTrait {
	fn name(&self) -> &'static str;
	fn context(&self) -> Context;
	fn signature(&self) -> &Signature<'static>;
	fn instructions(&self) -> &'static [&'static [Instruction]];

	fn inputs(&self) -> &'static [Type] { self.signature().inputs }
	fn outputs(&self) -> &'static [Type] { self.signature().outputs }
}

impl PrecompiledProcedureTrait for PrecompiledProcedure {
	fn name(&self) -> &'static str { self.0 }
	fn context(&self) -> Context { self.1 }
	fn signature(&self) -> &Signature<'static> { &self.2 }
	fn instructions(&self) -> &'static [&'static [Instruction]] { self.3 }
}

#[allow(unused)] const U: Context = Context::Universal;
#[allow(unused)] const G: Context = Context::Global;
#[allow(unused)] const N: Context = Context::Note;

pub static BUILTIN_FUNCTIONS: &[BuiltinFunction] = &[
	("atan2",      U, sig!([mono, mono] [mono]),          code![Atan2]),
	("ceil",       U, sig!([generic] [generic]),          code![Ceil]),
	("cos",        U, sig!([mono] [mono]),                code![Cos]),
	("exp2",       U, sig!([mono] [mono]),                code![Exp2]),
	("floor",      U, sig!([generic] [generic]),          code![Floor]),
	("gate",       N, sig!([] [mono bool]),               code![ReadNoteProperty(Gate)]),
	("gmdls",      U, sig!([mono, mono] [mono]),          code![GmDlsSample, Constant(0x30000000), Mul]),
	("index",      U, sig!([generic buffer] [mono]),      code![BufferIndex]),
	("key",        N, sig!([] [mono]),                    code![ReadNoteProperty(Key)]),
	("left",       U, sig!([stereo] [mono]),              code![Left]),
	("length",     U, sig!([generic buffer] [mono]),      code![BufferLength]),
	("max",        U, sig!([generic, generic] [generic]), code![Max]),
	("min",        U, sig!([generic, generic] [generic]), code![Min]),
	("mlog2",      U, sig!([mono, mono] [mono]),          code![Mlog2]),
	("random",     U, sig!([mono, mono] [mono]),          code![Random, Constant(0x30000000), Mul]),
	("right",      U, sig!([stereo] [mono]),              code![Right]),
	("round",      U, sig!([generic] [generic]),          code![Round]),
	("samplerate", U, sig!([] [mono]),                    code![SampleRate]),
	("sin",        U, sig!([mono] [mono]),                code![Sin]),
	("sqrt",       U, sig!([generic] [generic]),          code![Sqrt]),
	("tan",        U, sig!([mono] [mono]),                code![Tan]),
	("trunc",      U, sig!([generic] [generic]),          code![Trunc]),
	("velocity",   N, sig!([] [mono]),                    code![ReadNoteProperty(Velocity)]),
];

pub static BUILTIN_MODULES: &[BuiltinModule] = &[
	("cell",     U, sig!([static generic typeless, dynamic generic typeless] [dynamic generic typeless])),
	("delay",    U, sig!([static mono number, dynamic generic typeless] [dynamic generic typeless])),
	("dyndelay", U, sig!([static mono number, dynamic mono number, dynamic generic typeless] [dynamic generic typeless])),
];

pub static PRECOMPILED_FUNCTIONS: &[PrecompiledProcedure] = &[
	("center", U, sig!([stereo] [mono]), &[code![
		SplitRL,
		Add,
		Constant(0.5f32.to_bits()),
		Mul
	]]),
];

pub static PRECOMPILED_MODULES: &[PrecompiledProcedure] = &[
	("$autokill", N, sig!([stereo, stereo] [stereo]), &[code![
		Constant(0),
		CellInit
	], code![
		Constant(0x46000000), // Counter threshold
		StackLoad(1), // Copy of output
		Constant(0x46000000), ExpandStereo, Mul, Round, // 0 when small
		SplitRL, Or, // 0 when both channels small
		Constant(0), Eq, // True when small
		CellPush, And, // Preserve counter when small
		Constant(0x3F800000), Add, // Increment counter
		IfGreaterEq, // Has counter reached threshold?
		Kill, // Kill when counter reaches threshold
		EndIf,
		CellPop, // Update counter
		Pop, // Pop threshold
		Add // Add the output to the accumulator
	]]),
];

pub static REPETITION_COMBINATORS: &[(&'static str, f32, &'static [Instruction])] = &[
	("add", 0.0,               code![Add]),
	("max", f32::NEG_INFINITY, code![Max]),
	("min", f32::INFINITY,     code![Min]),
	("mul", 1.0,               code![Mul]),
];

pub trait OperatorSemantics {
	fn signature(&self) -> &'static Signature<'static>;
	fn instructions(&self) -> &'static [Instruction];
}

impl OperatorSemantics for UnOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}

	fn instructions(&self) -> &'static [Instruction] {
		self.kind.instructions()
	}
}

impl OperatorSemantics for UnOpKind {
	fn signature(&self) -> &'static Signature<'static> {
		use UnOpKind::*;
		match self {
			Neg => &sig!([generic number] [generic number]),
			Not => &sig!([generic bool] [generic bool]),
		}
	}

	// Instruction to execute with 0 and the unary operand as its binary operands
	fn instructions(&self) -> &'static [Instruction] {
		use UnOpKind::*;
		match self {
			Neg => code![Sub],
			Not => code![Eq],
		}

	}
}

impl OperatorSemantics for BinOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}

	fn instructions(&self) -> &'static [Instruction] {
		self.kind.instructions()
	}
}

impl OperatorSemantics for BinOpKind {
	fn signature(&self) -> &'static Signature<'static> {
		use BinOpKind::*;
		match self {
			Add       => &sig!([generic number, generic number] [generic number]),
			Sub       => &sig!([generic number, generic number] [generic number]),
			Mul       => &sig!([generic number, generic number] [generic number]),
			Div       => &sig!([generic number, generic number] [generic number]),
			And       => &sig!([generic bool, generic bool] [generic bool]),
			Or        => &sig!([generic bool, generic bool] [generic bool]),
			Xor       => &sig!([generic bool, generic bool] [generic bool]),
			Eq        => &sig!([generic number, generic number] [generic bool]),
			Neq       => &sig!([generic number, generic number] [generic bool]),
			Less      => &sig!([generic number, generic number] [generic bool]),
			LessEq    => &sig!([generic number, generic number] [generic bool]),
			Greater   => &sig!([generic number, generic number] [generic bool]),
			GreaterEq => &sig!([generic number, generic number] [generic bool]),
		}
	}

	fn instructions(&self) -> &'static [Instruction] {
		use BinOpKind::*;
		match self {
			Add       => code![Add],
			Sub       => code![Sub],
			Mul       => code![Mul],
			Div       => code![Div],
			And       => code![And],
			Or        => code![Or],
			Xor       => code![Xor],
			Eq        => code![Eq],
			Neq       => code![Neq],
			Less      => code![Less],
			LessEq    => code![LessEq],
			Greater   => code![Greater],
			GreaterEq => code![GreaterEq],
		}
	}
}
