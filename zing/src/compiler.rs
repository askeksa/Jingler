
use std::mem::replace;
use std::rc::Rc;

use regex::{Captures, Regex};

use lalrpop_util::ParseError;

use bytecode::bytecodes::Bytecode;

use crate::ast::*;
use crate::code_generator::generate_code;
use crate::names::Names;
use crate::type_inference::infer_types;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

/// Main compiler state, mainly concerned with error reporting.
pub struct Compiler<'input> {
	filename: &'input str,
	raw_input: &'input str,
	processed_input: Rc<str>, // Comments removed, semicolons added
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
	marker_max_length: usize,
}

impl Message {
	fn is_fatal(&self) -> bool {
		self.category.is_fatal()
	}
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

	fn is_fatal(self) -> bool {
		use MessageCategory::*;
		match self {
			SyntaxError => true,
			Error => true,
			InternalError => true,
			Warning => false,
			Context => false,
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
			let line_until_marker = &line[..msg.column];
			let indent = self.r_not_tab.replace_all(line_until_marker, " ");
			let marker = if msg.length <= msg.marker_max_length {
				"^".repeat(msg.length)
			} else {
				"^".repeat(msg.marker_max_length) + "..."
			};
			format!("{}:{}:{}: {}: {}\n\n{}\n{}{}\n",
				self.filename, msg.line + 1, msg.column + 1,
				msg.category.as_text(), msg.text,
				line,
				indent, marker)
		})
	}
}

/// Trait for things that can represent a range of positions for a diagnostic.
pub trait Location {
	fn pos_before(&self) -> usize;
	fn pos_after(&self) -> usize;
}

#[derive(Clone, Copy, Debug)]
pub struct PosRange {
	before: usize,
	after: usize,
}

impl PosRange {
	pub fn from(loc: &impl Location) -> Self {
		PosRange {
			before: loc.pos_before(),
			after: loc.pos_after(),
		}
	}
}

impl Location for PosRange {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.after }
}

/// (offset, length) pair.
impl Location for (usize, usize) {
	fn pos_before(&self) -> usize { self.0 }
	fn pos_after(&self) -> usize { self.0 + self.1 }
}

impl<'input> Location for Id<'input> {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.text.len() }
}

impl Location for UnOp {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.to_string().len() }
}

impl Location for BinOp {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.to_string().len() }
}

impl<'input> Location for Pattern<'input> {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.after }
}

impl<A: Location, B:Location> Location for (A, B) {
	fn pos_before(&self) -> usize { self.0.pos_before() }
	fn pos_after(&self) -> usize { self.1.pos_after() }
}

impl<'input> Location for Expression<'input> {
	fn pos_before(&self) -> usize {
		self.pos_before()
	}

	fn pos_after(&self) -> usize {
		self.pos_after()
	}
}

type CompilerOutput = Vec<Bytecode>;

impl<'input> Compiler<'input> {
	pub fn new(filename: &'input str, raw_input: &'input str) -> Compiler<'input> {
		let r_process = Regex::new(r"(?m:^(?:([ \t]*)([^#]+?)([;\\]?))?[ \t]*(?:#.*)?\r?$)").unwrap();
		let process_fn = |caps: &Captures| -> String {
			match (caps.get(1), caps.get(2), caps.get(3)) {
				(Some(indent), Some(text), Some(term)) => {
					// Insert semicolon if line is indented and does not end with
					// a semicolon or backslash. Remove trailing backslash.
					let semi = match (indent.as_str(), term.as_str()) {
						("", _) => term.as_str(),
						(_, "\\") => "",
						_ => ";",
					};
					format!("{}{}{}", indent.as_str(), text.as_str(), semi)
				},
				_ => format!(""),
			}
		};
		let processed_input = Rc::from(r_process.replace_all(raw_input, process_fn).as_ref());

		Compiler {
			filename,
			raw_input,
			processed_input,
			messages: vec![],
			r_line_break: Regex::new(r"\n|\r\n").unwrap(),
		}
	}

	pub fn compile(&mut self) -> Result<CompilerOutput, CompileError> {
		let processed_input = Rc::clone(&self.processed_input);
		let mut program = self.parse(&processed_input)?;
		let names = Names::find(&program, self)?;
		let signatures = infer_types(&mut program, &names, self)?;
		let code = generate_code(&program, &names, signatures, self)?;

		Ok(code)
	}

	fn parse<'ast>(&mut self, text: &'ast str) -> Result<Program<'ast>, CompileError> {
		zing::ProgramParser::new().parse(text).map_err(|err| {
			match err {
				ParseError::InvalidToken { location } => {
					self.report_syntax_error(&(location, 1), "Invalid token.");
				},
				ParseError::UnrecognizedEOF { location, expected: _ } => {
					self.report_syntax_error(&(location, 1), "Unexpected end of file.");
				},
				ParseError::UnrecognizedToken { token: (loc1, _, loc2), expected: _ } => {
					self.report_syntax_error(&(loc1, loc2 - loc1), "Unexpected token.");
				},
				ParseError::ExtraToken { token: (loc1, _, loc2) } => {
					self.report_syntax_error(&(loc1, loc2 - loc1), "Extra token.");
				},
				_ => self.report_internal_error(&(0, 0), "Unknown parse error."),
			}
			self.make_error()
		})
	}

	fn report(&mut self, loc: &dyn Location, category: MessageCategory, text: String) {
		let offset = loc.pos_before();
		let length = loc.pos_after() - offset;
		let (until_offset, from_offset) = self.processed_input.split_at(offset);
		let marker_max_length = self.r_line_break.find(from_offset).map(|m| m.start()).unwrap_or(0);
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
			marker_max_length,
		});
	}

	pub fn report_syntax_error(&mut self, loc: &dyn Location, text: impl Into<String>) {
		self.report(loc, MessageCategory::SyntaxError, text.into())
	}

	pub fn report_error(&mut self, loc: &dyn Location, text: impl Into<String>) {
		self.report(loc, MessageCategory::Error, text.into())
	}

	pub fn report_internal_error(&mut self, loc: &dyn Location, text: impl Into<String>) {
		self.report(loc, MessageCategory::InternalError, text.into())
	}

	pub fn report_warning(&mut self, loc: &dyn Location, text: impl Into<String>) {
		self.report(loc, MessageCategory::Warning, text.into())
	}

	pub fn report_context(&mut self, loc: &dyn Location, text: impl Into<String>) {
		self.report(loc, MessageCategory::Context, text.into())
	}

	fn make_error(&mut self) -> CompileError {
		CompileError {
			filename: self.filename.to_string(),
			messages: replace(&mut self.messages, vec![]),
			index: 0,
			r_not_tab: Regex::new(r"[^\t]").unwrap(),
		}
	}

	pub fn check_errors(&mut self) -> Result<(), CompileError> {
		self.if_no_errors(())
	}

	pub fn if_no_errors<T>(&mut self, value: T) -> Result<T, CompileError> {
		if self.messages.iter().any(|m| m.is_fatal()) {
			Err(self.make_error())
		} else {
			Ok(value)
		}
	}
}
