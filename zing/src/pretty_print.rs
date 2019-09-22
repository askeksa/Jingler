use std::fmt::{Display, Error, Formatter};

use crate::ast::*;

impl<'input> Display for Program<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		for declaration in &self.declarations {
			write!(f, "\n{}", declaration)?;
		}
		Ok(())
	}
}

impl<'input> Display for Declaration<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Declaration::*;
		match self {
			Procedure { kind, name, params, outputs, body } => {
				write!(f, "{} {}", kind, name)?;
				fmt_parenthesized_list(f, params)?;
				write!(f, " -> ")?;
				fmt_parenthesized_list(f, outputs)?;
				write!(f, "\n")?;
				for statement in body {
					write!(f, "    {}\n", statement)?;
				}
			},
		}
		Ok(())
	}
}

impl Display for ProcedureKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use ProcedureKind::*;
		match *self {
			Function => "function",
			Instrument => "instrument",
		}.fmt(f)
	}
}

impl<'input> Display for Statement<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Statement::*;
		match self {
			Assign { node, exp } => {
				fmt_parenthesized_list(f, node)?;
				write!(f, " = {};", exp)?;
			},
		}
		Ok(())
	}
}

impl<'input> Display for PatternItem<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "{}", self.variable)?;
		let colon = &mut false;
		fmt_with_colon(f, colon, &self.scope)?;
		fmt_with_colon(f, colon, &self.width)?;
		fmt_with_colon(f, colon, &self.value_type)?;
		Ok(())
	}
}

fn fmt_with_colon<T: Display>(f: &mut Formatter, colon: &mut bool, t_opt: &Option<T>) -> Result<(), Error> {
	if let Some(t) = t_opt {
		if !*colon {
			write!(f, ":")?;
			*colon = true;
		}
		write!(f, " {}", t)?;
	}
	Ok(())
}

impl<'input> Display for PatternVariable<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use PatternVariable::*;
		match self {
			Variable { name } => write!(f, "{}", name),
			Split { left, right } => write!(f, "[{}, {}]", left, right),
		}
	}
}

impl<'input> Display for Scope<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Scope::*;
		match self {
			Static => write!(f, "static"),
			Dynamic => write!(f, "dynamic"),
			Generic { name } => write!(f, "'{}", name),
		}
	}
}

impl<'input> Display for Width<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Width::*;
		match self {
			Mono => write!(f, "mono"),
			Stereo => write!(f, "stereo"),
			Generic { name } => write!(f, "''{}", name),
		}
	}
}

impl Display for ValueType {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use ValueType::*;
		match self {
			Number => write!(f, "number"),
			Bool => write!(f, "bool"),
			Buffer => write!(f, "buffer"),
		}
	}
}

#[derive(Clone, Copy)]
enum Precedence {
	Expression = 0,
	Or = 1,
	Xor = 2,
	And = 3,
	Compare = 4,
	Additive = 5,
	Multiplicative = 6,
	Unary = 7,
	Primary = 8,
}

impl Precedence {
	fn binary_next(&self) -> Precedence {
		use Precedence::*;
		match self {
			Or => Xor,
			Xor => And,
			And => Compare,
			Compare => Additive,
			Additive => Multiplicative,
			Multiplicative => Unary,
			_ => panic!(),
		}
	}
}

impl BinOpKind {
	fn precedence(&self) -> Precedence {
		use BinOpKind::*;
		match self {
			Or => Precedence::Or,
			Xor => Precedence::Xor,
			And => Precedence::And,
			Eq | Neq | Less | LessEq | Greater | GreaterEq => Precedence::Compare,
			Add | Sub => Precedence::Additive,
			Mul | Div => Precedence::Multiplicative,
		}
	}
}

impl Display for UnOpKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use UnOpKind::*;
		match self {
			Neg => "-",
			Not => "!",
		}.fmt(f)
	}
}

impl Display for BinOpKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use BinOpKind::*;
		match self {
			Add => "+",
			Sub => "-",
			Mul => "*",
			Div => "/",
			And => "&",
			Or => "|",
			Xor => "^",
			Eq => "==",
			Neq => "!=",
			Less => "<",
			LessEq => "<=",
			Greater => ">",
			GreaterEq => ">=",
		}.fmt(f)
	}
}

impl<'input> Display for Expression<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.fmt_with_precedence(f, Precedence::Expression)
	}
}

impl<'input> Expression<'input> {
	fn fmt_with_precedence(&self, f: &mut Formatter, p: Precedence) -> Result<(), Error> {
		let parenthesized = (self.precedence() as u8) < (p as u8);
		if parenthesized { f.write_str("(")?; }
		use Expression::*;
		match self {
			Number { value } => write!(f, "{}", value)?,
			Variable { name } => write!(f, "{}", name)?,
			UnOp { op, exp } => {
				write!(f, "{}", op)?;
				exp.fmt_with_precedence(f, Precedence::Primary)?;
			},
			BinOp { left, op, right } => {
				left.fmt_with_precedence(f, op.precedence())?;
				write!(f, " {} ", op)?;
				right.fmt_with_precedence(f, op.precedence().binary_next())?;
			},
			Call { name, args } => {
				write!(f, "{}", name)?;
				fmt_parenthesized_list(f, args)?;
			},
			Tuple { elements } => {
				fmt_parenthesized_list(f, elements)?;
			},
			Merge { left, right } => {
				write!(f, "[{}, {}]", left, right)?;
			},
			Property { exp, name } => {
				exp.fmt_with_precedence(f, Precedence::Primary)?;
				write!(f, ".{}", name)?;
			},
			TupleIndex { exp, index } => {
				exp.fmt_with_precedence(f, Precedence::Primary)?;
				write!(f, ".{}", index)?;
			},
			BufferIndex { exp, index } => {
				exp.fmt_with_precedence(f, Precedence::Primary)?;
				write!(f, "[{}]", index)?;
			},
		}
		if parenthesized { f.write_str(")")?; }
		Ok(())
	}

	fn precedence(&self) -> Precedence {
		use Expression::*;
		match self {
			Number { .. } => Precedence::Primary,
			Variable { .. } => Precedence::Primary,
			UnOp { .. } => Precedence::Unary,
			BinOp { op, .. } => op.precedence(),
			Call { .. } => Precedence::Primary,
			Tuple { .. } => Precedence::Primary,
			Merge { .. } => Precedence::Primary,
			Property { .. } => Precedence::Primary,
			TupleIndex { .. } => Precedence::Primary,
			BufferIndex { .. } => Precedence::Primary,
		}
	}
}

impl<'input> Display for Id<'input> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.text.fmt(f)
	}
}

fn fmt_parenthesized_list<T: Display>(f: &mut Formatter, list: &[T]) -> Result<(), Error> {
	write!(f, "(")?;
	let mut first = true;
	for element in list {
		if !first { write!(f, ", ")? }
		element.fmt(f)?;
		first = false;
	}
	write!(f, ")")?;
	Ok(())
}
