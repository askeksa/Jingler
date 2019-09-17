
#[derive(Debug)]
pub struct Program<'input> {
	pub declarations: Vec<Declaration<'input>>,
}

#[derive(Debug)]
pub enum Declaration<'input> {
	Procedure {
		kind: ProcedureKind,
		name: Id<'input>,
		params: Pattern<'input>,
		outputs: Pattern<'input>,
		body: Vec<Statement<'input>>,
	},
}

#[derive(Debug)]
pub enum ProcedureKind {
	Function,
	Instrument,
}

#[derive(Debug)]
pub enum Statement<'input> {
	Assign { node: Pattern<'input>, exp: Expression<'input> },
}

#[derive(Debug)]
pub enum Pattern<'input> {
	Variable { name: Id<'input>, scope: Scope<'input>, width: Width<'input> },
	Tuple { elements: Vec<Pattern<'input>> },
	Split { left: Box<Pattern<'input>>, right: Box<Pattern<'input>> },
}

#[derive(Debug)]
pub enum Scope<'input> {
	Static,
	Dynamic,
	Implicit,
	Generic { name: Id<'input> },
}

#[derive(Debug)]
pub enum Width<'input> {
	Mono,
	Stereo,
	Implicit,
	Generic { name: Id<'input> },
}

#[derive(Debug)]
pub enum Expression<'input> {
	Number { value: f64 },
	Variable { name: Id<'input> },
	UnOp { op: UnOpKind, exp: Box<Expression<'input>> },
	BinOp { left: Box<Expression<'input>>, op: BinOpKind, right: Box<Expression<'input>> },
	Call { name: Id<'input>, args: Vec<Expression<'input>> },
	Tuple { elements: Vec<Expression<'input>> },
	Merge { left: Box<Expression<'input>>, right: Box<Expression<'input>> },
	Property { exp: Box<Expression<'input>>, name: Id<'input> },
}

#[derive(Debug)]
pub enum UnOpKind {
	Neg, Not,
}

#[derive(Debug)]
pub enum BinOpKind {
	Add, Sub, Mul, Div, And, Or, Xor, Eq, Neq, Less, LessEq, Greater, GreaterEq,
}

#[derive(Debug)]
pub struct Id<'input> {
	pub pos: usize,
	pub text: &'input str,
}
