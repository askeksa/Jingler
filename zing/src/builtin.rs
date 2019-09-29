
use crate::ast::{BinOp, BinOpKind, Scope, Type, UnOp, UnOpKind, ValueType, Width};
use crate::names::Signature;

macro_rules! scope {
	{ static } => { Scope::Static };
	{ dynamic } => { Scope::Static };
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

macro_rules! sig {
	{ [ $($($p:ident)+),* ] [ $($($o:ident)+),* ] } => {
		Signature {
			params: &[$(type_spec!($($p)+)),*],
			outputs: &[$(type_spec!($($o)+)),*],
		}
	};
}

pub static BUILTIN_FUNCTIONS: &[(&'static str, Signature<'static>)] = &[
	("atan2", sig!([mono, mono] [mono])),
	("ceil",  sig!([generic] [generic])),
	("cos",   sig!([mono] [mono])),
	("exp2",  sig!([mono] [mono])),
	("floor", sig!([generic] [generic])),
	("log2",  sig!([mono] [mono])),
	("max",   sig!([generic, generic] [generic])),
	("min",   sig!([generic, generic] [generic])),
	("pow",   sig!([mono, mono] [mono])),
	("round", sig!([generic] [generic])),
	("sin",   sig!([mono] [mono])),
	("sqrt",  sig!([generic] [generic])),
	("tan",   sig!([mono] [mono])),
	("trunc", sig!([generic] [generic])),
];

pub static BUILTIN_MODULES: &[(&'static str, Signature<'static>)] = &[
	("cell",  sig!([static generic typeless, dynamic generic typeless] [dynamic generic typeless])),
	("delay", sig!([static mono number, dynamic generic typeless] [dynamic generic typeless]))
];

pub trait OperatorSignature {
	fn signature(&self) -> &'static Signature<'static>;
}

impl OperatorSignature for UnOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}
}

impl OperatorSignature for UnOpKind {
	fn signature(&self) -> &'static Signature<'static> {
		match self {
			UnOpKind::Neg => &sig!([generic number] [generic number]),
			UnOpKind::Not => &sig!([generic bool] [generic bool]),
		}
	}
}

impl OperatorSignature for BinOp {
	fn signature(&self) -> &'static Signature<'static> {
		self.kind.signature()
	}
}

impl OperatorSignature for BinOpKind {
	fn signature(&self) -> &'static Signature<'static> {
		match self {
			BinOpKind::Add       => &sig!([generic number, generic number] [generic number]),
			BinOpKind::Sub       => &sig!([generic number, generic number] [generic number]),
			BinOpKind::Mul       => &sig!([generic number, generic number] [generic number]),
			BinOpKind::Div       => &sig!([generic number, generic number] [generic number]),
			BinOpKind::And       => &sig!([generic bool, generic bool] [generic bool]),
			BinOpKind::Or        => &sig!([generic bool, generic bool] [generic bool]),
			BinOpKind::Xor       => &sig!([generic bool, generic bool] [generic bool]),
			BinOpKind::Eq        => &sig!([generic number, generic number] [generic bool]),
			BinOpKind::Neq       => &sig!([generic number, generic number] [generic bool]),
			BinOpKind::Less      => &sig!([generic number, generic number] [generic bool]),
			BinOpKind::LessEq    => &sig!([generic number, generic number] [generic bool]),
			BinOpKind::Greater   => &sig!([generic number, generic number] [generic bool]),
			BinOpKind::GreaterEq => &sig!([generic number, generic number] [generic bool]),
		}
	}
}
