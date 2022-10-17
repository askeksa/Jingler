
use program::instructions::*;

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

pub static BUILTIN_FUNCTIONS: &[(&'static str, Signature<'static>, &'static [Instruction])] = &[
	("atan2",      sig!([mono, mono] [mono]),          code![Atan2]),
	("ceil",       sig!([generic] [generic]),          code![Ceil]),
	("cos",        sig!([mono] [mono]),                code![Cos]),
	("exp2",       sig!([mono] [mono]),                code![Exp2]),
	("floor",      sig!([generic] [generic]),          code![Floor]),
	("gate",       sig!([] [mono bool]),               code![ReadNoteProperty(Gate)]),
	("gmdls",      sig!([mono, mono] [mono]),          code![GmDlsSample, Constant(0x30000000), Mul]),
	("key",        sig!([] [mono]),                    code![ReadNoteProperty(Key)]),
	("max",        sig!([generic, generic] [generic]), code![Max]),
	("min",        sig!([generic, generic] [generic]), code![Min]),
	("mlog2",      sig!([mono, mono] [mono]),          code![Mlog2]),
	("random",     sig!([mono, mono] [mono]),          code![Random, Constant(0x30000000), Mul]),
	("round",      sig!([generic] [generic]),          code![Round]),
	("samplerate", sig!([] [mono]),                    code![SampleRate]),
	("sin",        sig!([mono] [mono]),                code![Sin]),
	("sqrt",       sig!([generic] [generic]),          code![Sqrt]),
	("tan",        sig!([mono] [mono]),                code![Tan]),
	("trunc",      sig!([generic] [generic]),          code![Trunc]),
	("velocity",   sig!([] [mono]),                    code![ReadNoteProperty(Velocity)]),
];

pub static BUILTIN_MODULES: &[(&'static str, Signature<'static>)] = &[
	("cell",     sig!([static generic typeless, dynamic generic typeless] [dynamic generic typeless])),
	("delay",    sig!([static mono number, dynamic generic typeless] [dynamic generic typeless])),
	("dyndelay", sig!([static mono number, dynamic mono number, dynamic generic typeless] [dynamic generic typeless])),
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
