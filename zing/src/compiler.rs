
use regex::Regex;

use lalrpop_util::ParseError;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

pub struct Compiler<'input> {
	filename: &'input str,
	raw_input: &'input str,
	stripped_input: String, // Comments removed
	errors: Vec<Message>,

	r_line_break: Regex,
	r_not_tab: Regex,
}

pub struct Message {
	category: MessageCategory,
	line: usize,
	column: usize,
	length: usize,
	text: String,
}

#[derive(Clone, Copy)]
pub enum MessageCategory {
	SyntaxError,
	Error,
	InternalError,
	Warning,
}

impl MessageCategory {
	fn as_text(self) -> &'static str {
		use MessageCategory::*;
		match self {
			SyntaxError => "Syntax error",
			Error => "Error",
			InternalError => "Internal error",
			Warning => "Warning",
		}
	}
}

pub struct CompileError<'input> {
	compiler: &'input Compiler<'input>,
}

pub struct CompileErrorIterator<'input> {
	compiler: &'input Compiler<'input>,
	index: usize,
}

impl<'input> IntoIterator for CompileError<'input> {
	type Item = String;
	type IntoIter = CompileErrorIterator<'input>;

	fn into_iter(self) -> CompileErrorIterator<'input> {
		CompileErrorIterator {
			compiler: self.compiler,
			index: 0,
		}
	}
}

impl<'input> Iterator for CompileErrorIterator<'input> {
	type Item = String;

	fn next(&mut self) -> Option<String> {
		self.compiler.errors.get(self.index).map(|msg| {
			let lines = self.compiler.r_line_break.split(self.compiler.raw_input);
			let line = lines.skip(msg.line).next().unwrap_or("");
			let line_until_marker = &line[..line.len().min(msg.column)];
			let indent = self.compiler.r_not_tab.replace_all(line_until_marker, " ");
			let marker = "^".repeat(msg.length);
			self.index += 1;
			format!("{}:{}:{}: {}: {}\n\n{}\n{}{}\n",
				self.compiler.filename, msg.line + 1, msg.column + 1,
				msg.category.as_text(), msg.text,
				line,
				indent, marker)
		})
	}
}

type CompilerOutput = String; // TODO: Sensible compile result

impl<'input> Compiler<'input> {
	pub fn new(filename: &'input str, raw_input: &'input str) -> Compiler<'input> {
		let strip_comments = Regex::new(r"//[^\r\n]*").unwrap();
		let stripped_input = strip_comments.replace_all(raw_input, "").to_string();

		Compiler {
			filename,
			raw_input,
			stripped_input,
			errors: vec![],
			r_line_break: Regex::new(r"\r|\n|\r\n").unwrap(),
			r_not_tab: Regex::new(r"[^\t]").unwrap(),
		}
	}

	pub fn compile(&mut self) -> Result<CompilerOutput, CompileError> {
		let program = match zing::ProgramParser::new().parse(&self.stripped_input) {
			Ok(result) => result,
			Err(err) => {
				match err {
					ParseError::InvalidToken { location } => {
						self.report_syntax_error(location, 1, "Invalid token.");
					},
					ParseError::UnrecognizedEOF { location, expected: _ } => {
						self.report_syntax_error(location, 1, "Unexpected end of file.");
					},
					ParseError::UnrecognizedToken { token: (loc1, _, loc2), expected: _ } => {
						self.report_syntax_error(loc1, loc2 - loc1, "Unexpected token.");
					},
					ParseError::ExtraToken { token: (loc1, _, loc2) } => {
						self.report_syntax_error(loc1, loc2 - loc1, "Extra token.");
					},
					_ => self.report_internal_error(0, 0, "Unknown parse error."),
				}
				return Err(self.make_error());
			},
		};

		// TODO: Compile program

		self.check_errors()?;
		Ok(program.to_string())
	}

	fn report(&mut self, offset: usize, length: usize, category: MessageCategory, text: String) {
		let until_offset = &self.stripped_input[..self.stripped_input.len().min(offset)];
		let (line, column) = {
			match self.r_line_break.find_iter(until_offset).enumerate().last() {
				Some((count, last)) => (count + 1, offset - last.end()),
				None => (0, offset)
			}
		};
		self.errors.push(Message {
			category,
			line,
			column,
			length,
			text,
		});
	}

	#[allow(dead_code)]
	pub fn report_syntax_error<S: Into<String>>(&mut self, offset: usize, length: usize, text: S) {
		self.report(offset, length, MessageCategory::SyntaxError, text.into())
	}

	#[allow(dead_code)]
	pub fn report_error<S: Into<String>>(&mut self, offset: usize, length: usize, text: S) {
		self.report(offset, length, MessageCategory::Error, text.into())
	}

	#[allow(dead_code)]
	pub fn report_internal_error<S: Into<String>>(&mut self, offset: usize, length: usize, text: S) {
		self.report(offset, length, MessageCategory::InternalError, text.into())
	}

	#[allow(dead_code)]
	pub fn report_warning<S: Into<String>>(&mut self, offset: usize, length: usize, text: S) {
		self.report(offset, length, MessageCategory::Warning, text.into())
	}

	fn make_error(&'input self) -> CompileError<'input> {
		CompileError { compiler: &self }
	}

	fn check_errors(&self) -> Result<(), CompileError> {
		if !self.errors.is_empty() {
			Err(self.make_error())
		} else {
			Ok(())
		}
	}
}
