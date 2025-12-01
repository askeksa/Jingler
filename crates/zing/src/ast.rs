
type Pos = usize;

#[derive(Clone, Debug)]
pub struct Program {
	pub includes: Vec<Include>,
	pub parameters: Vec<Parameter>,
	pub procedures: Vec<Procedure>,
}

#[derive(Clone, Debug)]
pub struct Include {
	pub before: Pos,
	pub path: String,
	pub after: Pos,
}

#[derive(Clone, Debug)]
pub struct Parameter {
	pub name: Id,
	pub min: f64,
	pub max: f64,
	pub default: Option<f64>,
}

#[derive(Clone, Debug)]
pub struct Procedure {
	pub context: Context,
	pub kind: ProcedureKind,
	pub channels: Vec<Id>,
	pub name: Id,
	pub inputs: Pattern,
	pub outputs: Pattern,
	pub body: Vec<Statement>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Context {
	Universal,
	Global,
	Note,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProcedureKind {
	Module,
	Function,
	Instrument,
}

#[derive(Clone, Debug)]
pub enum Statement {
	Assign { node: Pattern, exp: Expression },
}

#[derive(Clone, Debug)]
pub struct Pattern {
	pub before: Pos,
	pub items: Vec<PatternItem>,
	pub after: Pos,
}

#[derive(Clone, Debug)]
pub struct PatternItem {
	pub name: Id,
	pub item_type: Type,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Type {
	pub scope: Option<Scope>,
	pub width: Option<Width>,
	pub value_type: Option<ValueType>,
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
	Bool,
	Buffer,
	Typeless,
}

impl Type {
	pub fn assignable_to(&self, to: &Type) -> bool {
		let scope_ok = match (self.scope, to.scope) {
			(Some(Scope::Static), Some(Scope::Dynamic)) => true,
			(None, _) => true,
			(from, to) => from == to,
		};
		let width_ok = match (self.value_type, self.width, to.width) {
			(Some(ValueType::Buffer), from, to) => from == to,
			(_, Some(Width::Mono), Some(Width::Stereo)) => true,
			(_, Some(Width::Mono), Some(Width::Generic)) => true,
			(_, from, to) => from == to,
		};
		let value_type_ok = self.value_type == to.value_type;

		scope_ok && width_ok && value_type_ok
	}

	pub fn inherit(&mut self, from: &Type) {
		self.scope = self.scope.or(from.scope);
		self.width = self.width.or(from.width);
		self.value_type = self.value_type.or(from.value_type);
	}
}

#[derive(Clone, Debug)]
pub enum Expression {
	Number { before: Pos, value: f64, after: Pos },
	Bool { before: Pos, value: bool },
	Variable { name: Id },
	UnOp { op: UnOp, exp: Box<Expression> },
	BinOp { left: Box<Expression>, op: BinOp, right: Box<Expression> },
	Conditional {
		condition: Box<Expression>,
		then: Box<Expression>,
		otherwise: Box<Expression>,
	},
	Call {
		before: Pos,
		channels: Vec<MidiChannel>,
		name: Id,
		args: Vec<Expression>,
		after: Pos
	},
	Tuple { before: Pos, elements: Vec<Expression>, after: Pos },
	Merge { before: Pos, left: Box<Expression>, right: Box<Expression>, after: Pos },
	TupleIndex { exp: Box<Expression>, index: u64, after: Pos },
	BufferIndex { exp: Box<Expression>, index: Box<Expression>, after: Pos },
	For {
		before: Pos,
		name: Id,
		count: Box<Expression>,
		combinator: Id,
		body: Box<Expression>,
	},
	Expand { exp: Box<Expression>, width: Width },
}

#[derive(Clone, Copy, Debug)]
pub struct UnOp {
	pub before: Pos,
	pub kind: UnOpKind,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOpKind {
	Neg, Not,
}

#[derive(Clone, Copy, Debug)]
pub struct BinOp {
	pub before: Pos,
	pub kind: BinOpKind,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
	Add, Sub, Mul, Div, And, Or, Xor, Eq, Neq, Less, LessEq, Greater, GreaterEq,
}

#[derive(Clone, Debug)]
pub enum MidiChannel {
	Value { channel: usize },
	Named { name: Id },
}

#[derive(Clone, Debug)]
pub struct Id {
	pub before: Pos,
	pub text: String,
}

impl Expression {
	pub fn pos_before(&self) -> usize {
		use Expression::*;
		match *self {
			Number { before, .. } => before,
			Bool { before, .. } => before,
			Variable { ref name, .. } => name.before,
			UnOp { op, .. } => op.before,
			BinOp { ref left, .. } => left.pos_before(),
			Conditional { ref condition, .. } => condition.pos_before(),
			Call { before, .. } => before,
			Tuple { before, .. } => before,
			Merge { before, .. } => before,
			TupleIndex { ref exp, .. } => exp.pos_before(),
			BufferIndex { ref exp, .. } => exp.pos_before(),
			For { before, .. } => before,
			Expand { ref exp, .. } => exp.pos_before(),
		}
	}

	pub fn pos_after(&self) -> usize {
		use Expression::*;
		match *self {
			Number { after, .. } => after,
			Bool { before, value } => before + if value { 4 } else { 5 },
			Variable { ref name, .. } => name.before + name.text.as_bytes().len(),
			UnOp { ref exp, .. } => exp.pos_after(),
			BinOp { ref right, .. } => right.pos_after(),
			Conditional { ref otherwise, .. } => otherwise.pos_after(),
			Call { after, .. } => after,
			Tuple { after, .. } => after,
			Merge { after, .. } => after,
			TupleIndex { after, .. } => after,
			BufferIndex { after, .. } => after,
			For { ref body, .. } => body.pos_after(),
			Expand { ref exp, .. } => exp.pos_after(),
		}
	}

	pub fn traverse<'ast>(&'ast self,
			pre: &mut impl FnMut(&'ast Expression),
			post: &mut impl FnMut(&'ast Expression)) {
		pre(self);
		use Expression::*;
		match self {
			Number { .. } => {},
			Bool { .. } => {},
			Variable { .. } => {},
			UnOp { exp, .. } => {
				exp.traverse(pre, post);
			},
			BinOp { left, right, .. } => {
				left.traverse(pre, post);
				right.traverse(pre, post);
			},
			Conditional { condition, then, otherwise } => {
				condition.traverse(pre, post);
				then.traverse(pre, post);
				otherwise.traverse(pre, post);
			},
			Call { args, .. } => {
				for arg in args {
					arg.traverse(pre, post);
				}
			},
			Tuple { elements, .. } => {
				for element in elements {
					element.traverse(pre, post);
				}
			},
			Merge { left, right, .. } => {
				left.traverse(pre, post);
				right.traverse(pre, post);
			},
			TupleIndex { exp, .. } => {
				exp.traverse(pre, post);
			},
			BufferIndex { exp, index, .. } => {
				exp.traverse(pre, post);
				index.traverse(pre, post);
			},
			For { count, body, .. } => {
				count.traverse(pre, post);
				body.traverse(pre, post);
			},
			Expand { exp, .. } => {
				exp.traverse(pre, post);
			},
		}
		post(self);
	}

	pub fn traverse_pre<'ast>(&'ast self,
			pre: &mut impl FnMut(&'ast Expression)) {
		self.traverse(pre, &mut |_| {});
	}

	pub fn traverse_post<'ast>(&'ast self,
			post: &mut impl FnMut(&'ast Expression)) {
		self.traverse(&mut |_| {}, post);
	}
}
