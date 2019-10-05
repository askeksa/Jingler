
#[derive(Clone, Debug)]
pub struct Program<'input> {
	pub declarations: Vec<Declaration<'input>>,
}

#[derive(Clone, Debug)]
pub enum Declaration<'input> {
	Procedure {
		kind: ProcedureKind,
		name: Id<'input>,
		inputs: Pattern<'input>,
		outputs: Pattern<'input>,
		body: Vec<Statement<'input>>,
	},
}

#[derive(Clone, Copy, Debug)]
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
	pub pos1: usize,
	pub items: Vec<PatternItem<'input>>,
	pub pos2: usize,
}

#[derive(Clone, Debug)]
pub struct PatternItem<'input> {
	pub variable: PatternVariable<'input>,
	pub item_type: Type,
}

#[derive(Clone, Debug)]
pub enum PatternVariable<'input> {
	Variable { name: Id<'input> },
	Split { left: Id<'input>, right: Id<'input> },
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

#[derive(Clone, Debug)]
pub enum Expression<'input> {
	Number { value: f64 },
	Variable { name: Id<'input> },
	UnOp { op: UnOp, exp: Box<Expression<'input>> },
	BinOp { left: Box<Expression<'input>>, op: BinOp, right: Box<Expression<'input>> },
	Call { name: Id<'input>, args: Vec<Expression<'input>> },
	Tuple { elements: Vec<Expression<'input>> },
	Merge { left: Box<Expression<'input>>, right: Box<Expression<'input>> },
	Property { exp: Box<Expression<'input>>, name: Id<'input> },
	TupleIndex { exp: Box<Expression<'input>>, index: u64 },
	BufferIndex {exp: Box<Expression<'input>>, index: Box<Expression<'input>> },
}

#[derive(Clone, Copy, Debug)]
pub struct UnOp {
	pub pos: usize,
	pub kind: UnOpKind,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOpKind {
	Neg, Not,
}

#[derive(Clone, Copy, Debug)]
pub struct BinOp {
	pub pos: usize,
	pub kind: BinOpKind,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
	Add, Sub, Mul, Div, And, Or, Xor, Eq, Neq, Less, LessEq, Greater, GreaterEq,
}

#[derive(Clone, Debug)]
pub struct Id<'input> {
	pub pos: usize,
	pub text: &'input str,
}
