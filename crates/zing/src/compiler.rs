
use std::mem::replace;
use std::rc::Rc;

use regex::Regex;

use lalrpop_util::ParseError;

use crate::ast::*;
use crate::code_generator::generate_code;
use crate::names::Names;
use crate::type_inference::infer_types;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

/// Main compiler state, mainly concerned with error reporting.
pub struct Compiler {
	filename: String,
	raw_input: String,
	processed_input: Rc<str>, // Comments removed
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

#[derive(Clone, Copy, Debug, Default)]
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
	fn pos_after(&self) -> usize { self.1 }
}

impl Location for Id {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.text.as_bytes().len() }
}

impl Location for UnOp {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.to_string().len() }
}

impl Location for BinOp {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.before + self.to_string().len() }
}

impl Location for Pattern {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.after }
}

impl<A: Location, B:Location> Location for (A, B) {
	fn pos_before(&self) -> usize { self.0.pos_before() }
	fn pos_after(&self) -> usize { self.1.pos_after() }
}

impl Location for Expression {
	fn pos_before(&self) -> usize {
		self.pos_before()
	}

	fn pos_after(&self) -> usize {
		self.pos_after()
	}
}

impl Procedure {
	pub fn midi_channels_location(&self) -> impl Location {
		match self.channels.first() {
			Some(channel) => (channel.pos_before(), self.name.pos_before()),
			None => (self.name.pos_before(), self.name.pos_after()),
		}
	}
}

impl Compiler {
	pub fn new(filename: String, raw_input: String) -> Compiler {
		// Remove comments
		let r_process = Regex::new(r"(?m:^([^#]*?)[ \t]*(?:#.*)?\r?$)").unwrap();
		let processed_input = Rc::from(r_process.replace_all(raw_input.as_str(), "$1").as_ref());

		Compiler {
			filename,
			raw_input,
			processed_input,
			messages: vec![],
			r_line_break: Regex::new(r"\n|\r\n").unwrap(),
		}
	}

	pub fn compile(&mut self) -> Result<ir::Program, CompileError> {
		let processed_input = Rc::clone(&self.processed_input);
		let mut program = self.parse(&processed_input)?;
		self.check_contexts(&mut program)?;
		let names = Names::find(&program, self)?;
		let (signatures, stored_widths, callees, precompiled_callees) = infer_types(&mut program, &names, self)?;
		let (procedures, main_static_proc_id, main_dynamic_proc_id, track_order)
			= generate_code(&program, &names, signatures, stored_widths, callees, precompiled_callees, self)?;
		let parameters = program.parameters.iter().map(|p| {
			ir::Parameter {
				name: p.name.to_string(),
				min: p.min as f32,
				max: p.max as f32,
				default: p.default.unwrap_or(p.min) as f32,
			}
		}).collect();

		Ok(ir::Program {
			parameters,
			procedures,
			main_static_proc_id,
			main_dynamic_proc_id,
			track_order,
		})
	}

	fn parse<'ast>(&mut self, text: &'ast str) -> Result<Program, CompileError> {
		zing::ProgramParser::new().parse(text).map_err(|err| {
			match err {
				ParseError::InvalidToken { location } => {
					self.report_syntax_error(&(location, location + 1), "Invalid token.");
				},
				ParseError::UnrecognizedEof { location, expected: _ } => {
					self.report_syntax_error(&(location, location), "Unexpected end of file.");
				},
				ParseError::UnrecognizedToken { token: (loc1, _, loc2), expected: _ } => {
					self.report_syntax_error(&(loc1, loc2), "Unexpected token.");
				},
				ParseError::ExtraToken { token: (loc1, _, loc2) } => {
					self.report_syntax_error(&(loc1, loc2), "Extra token.");
				},
				_ => self.report_internal_error(&(0, 0), "Unknown parse error."),
			}
			self.make_error()
		})
	}

	fn check_contexts(&mut self, program: &mut Program) -> Result<(), CompileError> {
		let mut found_main = false;
		for proc in &mut program.procedures {
			match (proc.context, proc.kind, proc.name.text.as_str()) {
				(Context::Global, ProcedureKind::Module, "main") => {
					found_main = true;
					if !proc.channels.is_empty() {
						self.report_error(&proc.midi_channels_location(),
							"'main' can't have midi channel inputs.");
					}
				},
				(_, _, "main") => {
					self.report_error(&proc.name, "'main' must be a global module.");
				},
				(Context::Global, ProcedureKind::Instrument, _) => {
					self.report_error(&proc.name, "Instruments can't be global.");
				},
				(Context::Note, ProcedureKind::Instrument, _) => {
					self.report_error(&proc.name, "Instruments have implicit note context.");
				},
				(_, ProcedureKind::Instrument, _) => {
					proc.context = Context::Note;
				},
				(Context::Global, ProcedureKind::Module, _) => {},
				_ => {
					if !proc.channels.is_empty() {
						self.report_error(&proc.midi_channels_location(),
							"Only global modules can have midi channel inputs.");
					}
				},
			}
		}
		if !found_main {
			self.report_error(&(0, 0), "No 'main' module.");
		}
		self.check_errors()
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
		let source_lines = self.r_line_break.split(&self.raw_input);
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
