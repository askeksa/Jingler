use std::fmt::{Display, Error, Formatter};

use crate::ast::*;

impl Display for Program {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		for parameter in &self.parameters {
			write!(f, "{}", parameter)?;
		}
		for procedure in &self.procedures {
			write!(f, "\n{}", procedure)?;
		}
		Ok(())
	}
}

impl Display for Parameter {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "parameter {} {} to {}", self.name, self.min, self.max)?;
		if let Some(default) = self.default {
			write!(f, " = {}", default)?;
		}
		write!(f, "\n")?;
		Ok(())
	}
}

impl Display for Procedure {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		if self.context != Context::Universal {
			write!(f, "{} ", self.context)?;
		}
		write!(f, "{} ", self.kind)?;
		for channel in &self.channels {
			write!(f, "{}::", channel)?;
		}
		write!(f, "{}", self.name)?;
		fmt_parenthesized_list(f, &self.inputs.items)?;
		write!(f, " -> ")?;
		fmt_parenthesized_list(f, &self.outputs.items)?;
		write!(f, "\n")?;
		for statement in &self.body {
			write!(f, "    {}\n", statement)?;
		}
		Ok(())
	}
}

impl Display for Context {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Context::*;
		match *self {
			Universal => "universal",
			Global => "global",
			Note => "note",
		}.fmt(f)
	}
}

impl Display for ProcedureKind {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use ProcedureKind::*;
		match *self {
			Module => "module",
			Function => "function",
			Instrument => "instrument",
		}.fmt(f)
	}
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Statement::*;
		match self {
			Assign { node, exp } => {
				fmt_parenthesized_list(f, &node.items)?;
				write!(f, " = {};", exp)?;
			},
		}
		Ok(())
	}
}

impl Display for PatternItem {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		write!(f, "{}", self.name)?;
		write!(f, "{}", self.item_type)?;
		Ok(())
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
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

impl Display for Scope {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Scope::*;
		match self {
			Static => write!(f, "static"),
			Dynamic => write!(f, "dynamic"),
		}
	}
}

impl Display for Width {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		use Width::*;
		match self {
			Mono => write!(f, "mono"),
			Stereo => write!(f, "stereo"),
			Generic => write!(f, "generic"),
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
			Typeless => write!(f, "typeless"),
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

impl BinOp {
	fn precedence(&self) -> Precedence {
		self.kind.precedence()
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

impl Display for UnOp {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.kind.fmt(f)
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

impl Display for BinOp {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.kind.fmt(f)
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

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.fmt_with_precedence(f, Precedence::Expression)
	}
}

impl Expression {
	fn fmt_with_precedence(&self, f: &mut Formatter, p: Precedence) -> Result<(), Error> {
		let parenthesized = (self.precedence() as u8) < (p as u8);
		if parenthesized { f.write_str("(")?; }
		use Expression::*;
		match self {
			Number { value, .. } => write!(f, "{}", value)?,
			Bool { value, .. } => write!(f, "{}", value)?,
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
			Conditional { condition, then, otherwise } => {
				condition.fmt_with_precedence(f, Precedence::Or)?;
				write!(f, " ? ")?;
				then.fmt_with_precedence(f, Precedence::Expression)?;
				write!(f, " : ")?;
				otherwise.fmt_with_precedence(f, Precedence::Expression)?;
			},
			Call { channels, name, args, .. } => {
				for channel in channels {
					write!(f, "{}::", channel)?;
				}
				write!(f, "{}", name)?;
				fmt_parenthesized_list(f, args)?;
			},
			Tuple { elements, .. } => {
				if let [single] = elements.as_slice() {
					single.fmt_with_precedence(f, p)?;
				} else {
					fmt_parenthesized_list(f, elements)?;
				}
			},
			Merge { left, right, .. } => {
				write!(f, "[{}, {}]", left, right)?;
			},
			TupleIndex { exp, index, .. } => {
				exp.fmt_with_precedence(f, Precedence::Primary)?;
				write!(f, ".{}", index)?;
			},
			BufferIndex { exp, index, .. } => {
				exp.fmt_with_precedence(f, Precedence::Primary)?;
				write!(f, "[{}]", index)?;
			},
			For { name, count, combinator, body, .. } => {
				write!(f, "for {} to ", name)?;
				count.fmt_with_precedence(f, Precedence::Expression)?;
				write!(f, " {} ", combinator)?;
				body.fmt_with_precedence(f, Precedence::Expression)?;
			},
			BufferInit { length, buffer_type, body, .. } => {
				write!(f, "for ")?;
				length.fmt_with_precedence(f, Precedence::Expression)?;
				write!(f, " {} ", buffer_type)?;
				body.fmt_with_precedence(f, Precedence::Expression)?;
			},
			Expand { exp, .. } => {
				write!(f, "[{}]", exp)?;
			},
		}
		if parenthesized { f.write_str(")")?; }
		Ok(())
	}

	fn precedence(&self) -> Precedence {
		use Expression::*;
		match self {
			Number { .. } => Precedence::Primary,
			Bool { .. } => Precedence::Primary,
			Variable { .. } => Precedence::Primary,
			UnOp { .. } => Precedence::Unary,
			BinOp { op, .. } => op.precedence(),
			Conditional { .. } => Precedence::Expression,
			Call { .. } => Precedence::Primary,
			Tuple { .. } => Precedence::Primary,
			Merge { .. } => Precedence::Primary,
			TupleIndex { .. } => Precedence::Primary,
			BufferIndex { .. } => Precedence::Primary,
			For { .. } => Precedence::Expression,
			BufferInit { .. } => Precedence::Expression,
			Expand { .. } => Precedence::Primary,
		}
	}
}

impl Display for MidiChannel {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		match self {
			MidiChannel::Value { channel } => write!(f, "{}", channel),
			MidiChannel::Named { name } => write!(f, "{}", name),
		}
	}
}

impl Display for Id {
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
