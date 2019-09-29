
use std::mem::replace;
use std::rc::Rc;

use regex::Regex;

use lalrpop_util::ParseError;

use crate::ast::*;
use crate::names::Names;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

/// Main compiler state, mainly concerned with error reporting.
pub struct Compiler<'input> {
	filename: &'input str,
	raw_input: &'input str,
	stripped_input: Rc<str>, // Comments removed
	messages: Vec<Message>,

	r_line_break: Regex,
}

/// A diagnostic message.
#[derive(Clone)]
pub struct Message {
	category: MessageCategory,
	line: usize,
	column: usize,
	length: usize,
	text: String,
	source_line: String,
}

/// Category / severity of a diagnostic.
#[derive(Clone, Copy)]
pub enum MessageCategory {
	SyntaxError,
	Error,
	InternalError,
	Warning,
	Context,
}

impl MessageCategory {
	fn as_text(self) -> &'static str {
		use MessageCategory::*;
		match self {
			SyntaxError => "Syntax error",
			Error => "Error",
			InternalError => "Internal error",
			Warning => "Warning",
			Context => "Context",
		}
	}
}

/// Error return from the compiler.
///
/// Iterate over it to get diagnostic messages.
pub struct CompileError {
	filename: String,
	messages: Vec<Message>,
	index: usize,

	r_not_tab: Regex,
}

impl Iterator for CompileError {
	type Item = String;

	fn next(&mut self) -> Option<String> {
		let index = self.index;
		self.index += 1;
		self.messages.get(index).map(|msg| {
			let line = &msg.source_line;
			let line_until_marker = &line[..line.len().min(msg.column)];
			let indent = self.r_not_tab.replace_all(line_until_marker, " ");
			let marker = "^".repeat(msg.length);
			format!("{}:{}:{}: {}: {}\n\n{}\n{}{}\n",
				self.filename, msg.line + 1, msg.column + 1,
				msg.category.as_text(), msg.text,
				line,
				indent, marker)
		})
	}
}

/// Trait for things that can represent a location and length for a diagnostic.
pub trait Location {
	fn offset(&self) -> usize;
	fn length(&self) -> usize;
}

impl Location for (usize, usize) {
	fn offset(&self) -> usize { self.0 }
	fn length(&self) -> usize { self.1 }
}

impl<'input> Location for &Id<'input> {
	fn offset(&self) -> usize { self.pos }
	fn length(&self) -> usize { self.text.len() }
}

impl Location for &UnOp {
	fn offset(&self) -> usize { self.pos }
	fn length(&self) -> usize { self.to_string().len() }
}

impl Location for &BinOp {
	fn offset(&self) -> usize { self.pos }
	fn length(&self) -> usize { self.to_string().len() }
}

type CompilerOutput = String; // TODO: Sensible compile result

impl<'input> Compiler<'input> {
	pub fn new(filename: &'input str, raw_input: &'input str) -> Compiler<'input> {
		let strip_comments = Regex::new(r"//[^\r\n]*").unwrap();
		let stripped_input = Rc::from(strip_comments.replace_all(raw_input, "").as_ref());

		Compiler {
			filename,
			raw_input,
			stripped_input,
			messages: vec![],
			r_line_break: Regex::new(r"\r|\n|\r\n").unwrap(),
		}
	}

	pub fn compile(&mut self) -> Result<CompilerOutput, CompileError> {
		let stripped_input = Rc::clone(&self.stripped_input);
		let program = self.parse(&stripped_input)?;
		let names = Names::find(&program, self)?;

		// TODO: Compile program

		self.check_errors()?;
		Ok(program.to_string())
	}

	fn parse<'ast>(&mut self, text: &'ast str) -> Result<Program<'ast>, CompileError> {
		zing::ProgramParser::new().parse(text).map_err(|err| {
			match err {
				ParseError::InvalidToken { location } => {
					self.report_syntax_error((location, 1), "Invalid token.");
				},
				ParseError::UnrecognizedEOF { location, expected: _ } => {
					self.report_syntax_error((location, 1), "Unexpected end of file.");
				},
				ParseError::UnrecognizedToken { token: (loc1, _, loc2), expected: _ } => {
					self.report_syntax_error((loc1, loc2 - loc1), "Unexpected token.");
				},
				ParseError::ExtraToken { token: (loc1, _, loc2) } => {
					self.report_syntax_error((loc1, loc2 - loc1), "Extra token.");
				},
				_ => self.report_internal_error((0, 0), "Unknown parse error."),
			}
			self.make_error()
		})
	}

	fn report(&mut self, offset: usize, length: usize, category: MessageCategory, text: String) {
		let until_offset = &self.stripped_input[..self.stripped_input.len().min(offset)];
		let (line, column) = {
			match self.r_line_break.find_iter(until_offset).enumerate().last() {
				Some((count, last)) => (count + 1, offset - last.end()),
				None => (0, offset)
			}
		};
		let source_lines = self.r_line_break.split(self.raw_input);
		let source_line = source_lines.skip(line).next().unwrap_or("").to_string();

		self.messages.push(Message {
			category,
			line,
			column,
			length,
			text,
			source_line,
		});
	}

	#[allow(dead_code)]
	pub fn report_syntax_error(&mut self, loc: impl Location, text: impl Into<String>) {
		self.report(loc.offset(), loc.length(), MessageCategory::SyntaxError, text.into())
	}

	#[allow(dead_code)]
	pub fn report_error(&mut self, loc: impl Location, text: impl Into<String>) {
		self.report(loc.offset(), loc.length(), MessageCategory::Error, text.into())
	}

	#[allow(dead_code)]
	pub fn report_internal_error(&mut self, loc: impl Location, text: impl Into<String>) {
		self.report(loc.offset(), loc.length(), MessageCategory::InternalError, text.into())
	}

	#[allow(dead_code)]
	pub fn report_warning(&mut self, loc: impl Location, text: impl Into<String>) {
		self.report(loc.offset(), loc.length(), MessageCategory::Warning, text.into())
	}

	#[allow(dead_code)]
	pub fn report_context(&mut self, loc: impl Location, text: impl Into<String>) {
		self.report(loc.offset(), loc.length(), MessageCategory::Context, text.into())
	}

	fn make_error(&mut self) -> CompileError {
		CompileError {
			filename: self.filename.to_string(),
			messages: replace(&mut self.messages, vec![]),
			index: 0,
			r_not_tab: Regex::new(r"[^\t]").unwrap(),
		}
	}

	fn check_errors(&mut self) -> Result<(), CompileError> {
		self.if_not_error(())
	}

	pub fn if_not_error<T>(&mut self, value: T) -> Result<T, CompileError> {
		if !self.messages.is_empty() {
			Err(self.make_error())
		} else {
			Ok(value)
		}
	}
}
