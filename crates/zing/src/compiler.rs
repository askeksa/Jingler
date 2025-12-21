
use std::mem::replace;
use std::rc::Rc;

use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

use lalrpop_util::ParseError;

use crate::ast::*;
use crate::code_generator::generate_code;
use crate::names::Names;
use crate::type_inference::infer_types;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

/// A single source file.
pub struct Source {
	pub filename: String,
	pub raw_input: String,
	pub processed_input: Rc<str>, // Comments removed
	pub start_offset: usize,
}

impl Source {
	pub fn new(filename: String, raw_input: String, start_offset: usize) -> Source {
		// Remove comments
		let mut processed = String::with_capacity(raw_input.len());
		for line in raw_input.lines() {
			let line = match line.find('#') {
				Some(i) => &line[..i],
				None => line,
			};
			processed.push_str(line);
			processed.push('\n');
		}
		let processed_input = Rc::from(processed);

		Source {
			filename,
			raw_input,
			processed_input,
			start_offset,
		}
	}
}

/// Main compiler state, mainly concerned with error reporting.
pub struct Compiler {
	sources: Vec<Source>,
	loaded_files: HashSet<PathBuf>,
	messages: Vec<Message>,
}

/// A diagnostic message.
#[derive(Clone)]
pub struct Message {
	category: MessageCategory,
	line: usize,
	column: usize,
	length: usize,
	text: String,
	filename: String,
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
	messages: Vec<Message>,
	index: usize,
}

impl Iterator for CompileError {
	type Item = String;

	fn next(&mut self) -> Option<String> {
		let index = self.index;
		self.index += 1;
		self.messages.get(index).map(|msg| {
			let line = &msg.source_line;
			let line_until_marker = &line[..msg.column];
			let indent = line_until_marker.replace(|c| c != '\t', " ");
			let marker = if msg.length <= msg.marker_max_length {
				"^".repeat(msg.length)
			} else {
				"^".repeat(msg.marker_max_length) + "..."
			};
			format!("{}:{}:{}: {}: {}\n\n{}\n{}{}\n",
				msg.filename, msg.line + 1, msg.column + 1,
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

impl Location for Include {
	fn pos_before(&self) -> usize { self.before }
	fn pos_after(&self) -> usize { self.after }
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
		let main_source = Source::new(filename.clone(), raw_input, 0);

		let mut loaded_files = HashSet::new();
		if let Ok(path) = fs::canonicalize(Path::new(&filename)) {
			loaded_files.insert(path);
		}

		Compiler {
			sources: vec![main_source],
			loaded_files,
			messages: vec![],
		}
	}

	pub fn compile(&mut self) -> Result<ir::Program, CompileError> {
		let processed_input = Rc::clone(&self.sources[0].processed_input);
		let mut program = self.parse(&processed_input, 0)?;
		self.process_includes(&mut program)?;
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

	fn process_includes(&mut self, program: &mut Program) -> Result<(), CompileError> {
		let mut include_queue: VecDeque<Include> = program.includes.drain(..).collect();
		while let Some(include) = include_queue.pop_front() {
			// Resolve included file path relative to the source file containing the include
			let source_index = self.source_index(include.before);
			let filename = self.sources[source_index].filename.clone();
			let base_path = Path::new(&filename).parent().ok_or_else(|| {
				self.report_error(&include, "Could not find parent directory.");
				self.make_error()
			})?;
			let resolved_path = base_path.join(&include.path);

			// Skip if already included
			if let Ok(canonical_path) = fs::canonicalize(&resolved_path) {
				if !self.loaded_files.insert(canonical_path) {
					continue;
				}
			}

			// Read included file
			let raw_input = fs::read_to_string(&resolved_path).map_err(|e| {
				self.report_error(&include, format!("Could not read file '{}': {}", resolved_path.display(), e));
				self.make_error()
			})?;

			// Add included file to sources
			let start_offset = self.sources.last().map(|s| s.start_offset + s.processed_input.len()).unwrap_or(0);
			let source = Source::new(
				resolved_path.to_string_lossy().to_string(),
				raw_input,
				start_offset,
			);
			let processed_input = Rc::clone(&source.processed_input);
			self.sources.push(source);

			// Parse included file and merge it into the main program
			let mut included_program = self.parse(&processed_input, start_offset)?;
			program.parameters.splice(0..0, included_program.parameters);
			program.procedures.splice(0..0, included_program.procedures);
			include_queue.extend(included_program.includes.drain(..));

			program.includes.push(include);
		}
		Ok(())
	}

	fn source_index(&self, pos: usize) -> usize {
		self.sources.partition_point(|s| s.start_offset <= pos).saturating_sub(1)
	}

	fn parse<'ast>(&mut self, text: &'ast str, start_offset: usize) -> Result<Program, CompileError> {
		// Prepend spaces to offset token locations. In the absence of support in LALRPOP
		// for setting the initial location, this seems like the cleanest solution.
		let offset_text = " ".repeat(start_offset) + text;

		zing::ProgramParser::new().parse(&offset_text).map_err(|err| {
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
		let pos = loc.pos_before();
		let length = loc.pos_after() - pos;
		let source_index = self.source_index(pos);
		let source = &self.sources[source_index];
		let offset = pos - source.start_offset;

		let (until_offset, from_offset) = source.processed_input.split_at(offset);
		let marker_max_length = from_offset.find('\n').unwrap_or(from_offset.len());
		let (line, column) = {
			match until_offset.lines().enumerate().last() {
				Some((count, last)) => (count, last.len()),
				None => (0, offset)
			}
		};
		let source_line = source.raw_input.lines().skip(line).next().unwrap_or("").to_string();

		self.messages.push(Message {
			category,
			line,
			column,
			length,
			text,
			filename: source.filename.clone(),
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
			messages: replace(&mut self.messages, vec![]),
			index: 0,
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
