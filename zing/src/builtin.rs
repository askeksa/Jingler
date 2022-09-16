
use program::bytecodes::*;

use crate::ast::{BinOp, BinOpKind, Scope, Type, UnOp, UnOpKind, ValueType, Width};
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

pub static BUILTIN_FUNCTIONS: &[(&'static str, Signature<'static>, &'static [Bytecode])] = &[
	("atan2",      sig!([mono, mono] [mono]),          bc![Atan2]),
	("ceil",       sig!([generic] [generic]),          bc![Ceil]),
	("cos",        sig!([mono] [mono]),                bc![Cos]),
	("exp2",       sig!([mono] [mono]),                bc![Exp2]),
	("floor",      sig!([generic] [generic]),          bc![Floor]),
	("gate",       sig!([] [mono bool]),               bc![ReadNoteProperty(Gate)]),
	("gmdls",      sig!([mono, mono] [mono]),          bc![GmDlsSample, Constant(0x38000000), Mul]),
	("gmdlslen",   sig!([mono] [mono]),                bc![GmDlsLength]),
	("key",        sig!([] [mono]),                    bc![ReadNoteProperty(Key)]),
	("max",        sig!([generic, generic] [generic]), bc![Max]),
	("min",        sig!([generic, generic] [generic]), bc![Min]),
	("mlog2",      sig!([mono, mono] [mono]),          bc![Mlog2]),
	("random",     sig!([mono, mono] [mono]),          bc![Random, Constant(0x30000000), Mul]),
	("round",      sig!([generic] [generic]),          bc![Round]),
	("samplerate", sig!([] [mono]),                    bc![SampleRate]),
	("sin",        sig!([mono] [mono]),                bc![Sin]),
	("sqrt",       sig!([generic] [generic]),          bc![Sqrt]),
	("tan",        sig!([mono] [mono]),                bc![Tan]),
	("trunc",      sig!([generic] [generic]),          bc![Trunc]),
	("velocity",   sig!([] [mono]),                    bc![ReadNoteProperty(Velocity)]),
];

pub static BUILTIN_MODULES: &[(&'static str, Signature<'static>)] = &[
	("cell",     sig!([static generic typeless, dynamic generic typeless] [dynamic generic typeless])),
	("delay",    sig!([static mono number, dynamic generic typeless] [dynamic generic typeless])),
	("dyndelay", sig!([static mono number, dynamic mono number, dynamic generic typeless] [dynamic generic typeless])),
];

pub static REPETITION_COMBINATORS: &[(&'static str, f32, &'static [Bytecode])] = &[
	("add", 0.0,               bc![Add]),
	("max", f32::NEG_INFINITY, bc![Max]),
	("min", f32::INFINITY,     bc![Min]),
	("mul", 1.0,               bc![Mul]),
];

pub trait OperatorSemantics {
	fn signature(&self) -> &'static Signature<'static>;
	fn bytecodes(&self) -> &'static [Bytecode];
}

impl OperatorSemantics for UnOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}

	fn bytecodes(&self) -> &'static [Bytecode] {
		self.kind.bytecodes()
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
	fn bytecodes(&self) -> &'static [Bytecode] {
		use UnOpKind::*;
		match self {
			Neg => bc![Sub],
			Not => bc![Eq],
		}

	}
}

impl OperatorSemantics for BinOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}

	fn bytecodes(&self) -> &'static [Bytecode] {
		self.kind.bytecodes()
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

	fn bytecodes(&self) -> &'static [Bytecode] {
		use BinOpKind::*;
		match self {
			Add       => bc![Add],
			Sub       => bc![Sub],
			Mul       => bc![Mul],
			Div       => bc![Div],
			And       => bc![And],
			Or        => bc![Or],
			Xor       => bc![Xor],
			Eq        => bc![Eq],
			Neq       => bc![Neq],
			Less      => bc![Less],
			LessEq    => bc![LessEq],
			Greater   => bc![Greater],
			GreaterEq => bc![GreaterEq],
		}
	}
}
