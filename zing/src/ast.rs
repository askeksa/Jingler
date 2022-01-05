
type Pos = usize;

#[derive(Clone, Debug)]
pub struct Program<'input> {
	pub procedures: Vec<Procedure<'input>>,
}

#[derive(Clone, Debug)]
pub struct Procedure<'input> {
	pub kind: ProcedureKind,
	pub name: Id<'input>,
	pub inputs: Pattern<'input>,
	pub outputs: Pattern<'input>,
	pub body: Vec<Statement<'input>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProcedureKind {
	Module,
	Function,
	Instrument,
}

#[derive(Clone, Debug)]
pub enum Statement<'input> {
	Assign { node: Pattern<'input>, exp: Expression<'input> },
}

#[derive(Clone, Debug)]
pub struct Pattern<'input> {
	pub before: Pos,
	pub items: Vec<PatternItem<'input>>,
	pub after: Pos,
}

#[derive(Clone, Debug)]
pub struct PatternItem<'input> {
	pub name: Id<'input>,
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
pub enum Expression<'input> {
	Number { before: Pos, value: f64, after: Pos },
	Bool { before: Pos, value: bool },
	Variable { name: Id<'input> },
	UnOp { op: UnOp, exp: Box<Expression<'input>> },
	BinOp { left: Box<Expression<'input>>, op: BinOp, right: Box<Expression<'input>> },
	Conditional {
		condition: Box<Expression<'input>>,
		then: Box<Expression<'input>>,
		otherwise: Box<Expression<'input>>,
	},
	Call { name: Id<'input>, args: Vec<Expression<'input>>, after: Pos },
	Tuple { before: Pos, elements: Vec<Expression<'input>>, after: Pos },
	Merge { before: Pos, left: Box<Expression<'input>>, right: Box<Expression<'input>>, after: Pos },
	Property { exp: Box<Expression<'input>>, name: Id<'input> },
	TupleIndex { exp: Box<Expression<'input>>, index: u64, after: Pos },
	BufferIndex { exp: Box<Expression<'input>>, index: Box<Expression<'input>>, after: Pos },
	For {
		before: Pos,
		name: Id<'input>,
		start: Box<Expression<'input>>,
		end: Box<Expression<'input>>,
		combinator: Id<'input>,
		body: Box<Expression<'input>>,
	},
	Expand { exp: Box<Expression<'input>> },
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
pub struct Id<'input> {
	pub before: Pos,
	pub text: &'input str,
}

impl<'input> Expression<'input> {
	pub fn pos_before(&self) -> usize {
		use Expression::*;
		match *self {
			Number { before, .. } => before,
			Bool { before, .. } => before,
			Variable { ref name, .. } => name.before,
			UnOp { op, .. } => op.before,
			BinOp { ref left, .. } => left.pos_before(),
			Conditional { ref condition, .. } => condition.pos_before(),
			Call { ref name, .. } => name.before,
			Tuple { before, .. } => before,
			Merge { before, .. } => before,
			Property { ref exp, .. } => exp.pos_before(),
			TupleIndex { ref exp, .. } => exp.pos_before(),
			BufferIndex { ref exp, .. } => exp.pos_before(),
			For { before, .. } => before,
			Expand { ref exp } => exp.pos_before(),
		}
	}

	pub fn pos_after(&self) -> usize {
		use Expression::*;
		match *self {
			Number { after, .. } => after,
			Bool { before, value } => before + if value { 4 } else { 5 },
			Variable { ref name, .. } => name.before + name.text.len(),
			UnOp { ref exp, .. } => exp.pos_after(),
			BinOp { ref right, .. } => right.pos_after(),
			Conditional { ref otherwise, .. } => otherwise.pos_after(),
			Call { after, .. } => after,
			Tuple { after, .. } => after,
			Merge { after, .. } => after,
			Property { ref name, .. } => name.before + name.text.len(),
			TupleIndex { after, .. } => after,
			BufferIndex { after, .. } => after,
			For { ref body, .. } => body.pos_after(),
			Expand { ref exp } => exp.pos_after(),
		}
	}

	pub fn traverse(&self,
			pre: &mut impl FnMut(&Expression<'input>),
			post: &mut impl FnMut(&Expression<'input>)) {
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
			Property { exp, .. } => {
				exp.traverse(pre, post);
			},
			TupleIndex { exp, .. } => {
				exp.traverse(pre, post);
			},
			BufferIndex { exp, index, .. } => {
				exp.traverse(pre, post);
				index.traverse(pre, post);
			},
			For { start, end, body, .. } => {
				start.traverse(pre, post);
				end.traverse(pre, post);
				body.traverse(pre, post);
			},
			Expand { exp } => {
				exp.traverse(pre, post);
			},
		}
		post(self);
	}

	pub fn traverse_pre(&self,
			pre: &mut impl FnMut(&Expression<'input>)) {
		self.traverse(pre, &mut |_| {});
	}

	pub fn traverse_post(&self,
			post: &mut impl FnMut(&Expression<'input>)) {
		self.traverse(&mut |_| {}, post);
	}
}
