//! Keeps the Zing documentation honest:
//!
//! - Every ```` ```zing ```` block in ZING.md compiles. Blocks contain no boilerplate; the test
//!   adds it (see `wrap_block`). ```` ```zing main ```` blocks are the body of `main`, and
//!   ```` ```zing error ```` blocks must fail, with the text after a `# Error:` comment, if any.
//! - Every file in the top-level `zing/` directory compiles.
//! - The "Standard library" chapter of ZING.md lists exactly the public members of
//!   `zing/base.zing`: same sections, members, order, header lines and comments.

use std::fs;
use std::path::{Path, PathBuf};

use zing::compiler::Compiler;

fn repo_root() -> PathBuf {
	Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap()
}

fn library_dir() -> PathBuf {
	repo_root().join("zing")
}

fn read(path: &Path) -> String {
	fs::read_to_string(path).unwrap_or_else(|e| panic!("Could not read {}: {}", path.display(), e))
}

/// Compile `src` as if it were a file at `path` (which need not exist), so that
/// includes resolve relative to its directory.
fn compile(path: &Path, src: &str) -> Result<(), String> {
	Compiler::new(path.to_string_lossy().into_owned(), src.to_string())
		.compile()
		.map(|_| ())
		.map_err(|errors| errors.collect::<Vec<_>>().join("\n"))
}

fn normalize_whitespace(s: &str) -> String {
	s.split_whitespace().collect::<Vec<_>>().join(" ")
}

// ============================================================
// Member headers
// ============================================================

/// A member header line, e.g. `note module adsr(in: generic, a_time: static mono) -> out: generic`.
struct Header {
	line: String,
	context: String,
	kind: String,
	midi_inputs: usize,
	name: String,
	/// The type text of each input, e.g. "static mono".
	inputs: Vec<String>,
	/// The type text of each output.
	outputs: Vec<String>,
}

fn parse_header(line: &str) -> Option<Header> {
	let mut rest = line.trim();
	let mut context = String::new();
	let kind = loop {
		let (word, after) = rest.split_once(char::is_whitespace)?;
		rest = after.trim_start();
		match word {
			"external" => {},
			"global" | "note" => context = word.to_string(),
			"function" | "module" | "instrument" => break word.to_string(),
			_ => return None,
		}
	};
	let (name_part, after_name) = rest.split_once('(')?;
	let (inputs, after_inputs) = after_name.split_once(')')?;
	let outputs = after_inputs.trim().strip_prefix("->")?.trim();
	let outputs = outputs.strip_prefix('(').and_then(|o| o.strip_suffix(')')).unwrap_or(outputs);
	let types = |list: &str| -> Vec<String> {
		list.split(',')
			.filter(|item| !item.trim().is_empty())
			.map(|item| item.split_once(':').map(|(_, t)| t.trim().to_string()).unwrap_or_default())
			.collect()
	};
	Some(Header {
		line: normalize_whitespace(line),
		context,
		kind,
		midi_inputs: name_part.matches("::").count(),
		name: name_part.rsplit("::").next().unwrap().trim().to_string(),
		inputs: types(inputs),
		outputs: types(outputs),
	})
}

fn headers(src: &str) -> Vec<Header> {
	src.lines().filter_map(parse_header).collect()
}

/// A `main` that calls every member in `headers` (except `main`) once, with zero arguments,
/// so that the compiler generates code for all of them, not only checks their types.
/// Note members are called from an extra instrument.
fn generate_callers(headers: &[Header]) -> String {
	fn argument(input_type: &str) -> &'static str {
		if input_type.contains("bool") { "false" }
		else if input_type.contains("buffer") { "{0}" }
		else { "0" }
	}
	fn number(var: &str, output_type: &str) -> String {
		if output_type.contains("bool") { format!("({var} ? 1 : 0)") }
		else if output_type.contains("buffer") { format!("length({var})") }
		else { var.to_string() }
	}

	let mut main_body = String::new();
	let mut main_terms = vec![];
	let mut note_body = String::new();
	let mut note_terms = vec![];
	let mut channels = (1..=16).cycle();
	for (i, header) in headers.iter().enumerate() {
		if header.name == "main" || header.outputs.is_empty() {
			continue;
		}
		let prefix = if header.kind == "instrument" {
			format!("{}::", channels.next().unwrap())
		} else {
			"1::".repeat(header.midi_inputs)
		};
		let args = header.inputs.iter().map(|t| argument(t)).collect::<Vec<_>>().join(", ");
		let vars = (0..header.outputs.len()).map(|j| format!("doc_{i}_{j}")).collect::<Vec<_>>();
		let statement = format!("\t{} = {prefix}{}({args})\n", vars.join(", "), header.name);
		let terms = vars.iter().zip(&header.outputs).map(|(v, t)| number(v, t));
		if header.context == "note" {
			note_body += &statement;
			note_terms.extend(terms);
		} else {
			main_body += &statement;
			main_terms.extend(terms);
		}
	}

	let mut src = String::new();
	if !note_terms.is_empty() {
		src += "\ninstrument doc_note_members() -> out: stereo\n";
		src += &note_body;
		src += &format!("\tout = 0 + {}\n", note_terms.join(" + "));
		main_terms.push(format!("{}::doc_note_members()", channels.next().unwrap()));
	}
	src += "\nglobal module main() -> out: stereo\n";
	src += &main_body;
	src += &format!("\tout = 0 + {}\n", if main_terms.is_empty() { "0".to_string() } else { main_terms.join(" + ") });
	src
}

fn indent(lines: &[&str]) -> String {
	lines.iter().map(|line| format!("\t{line}\n")).collect()
}

// ============================================================
// Examples in ZING.md
// ============================================================

struct Block {
	/// Line number of the opening fence in ZING.md.
	line: usize,
	mode: String,
	code: String,
}

fn zing_blocks(markdown: &str) -> Vec<Block> {
	let mut blocks = vec![];
	let mut current: Option<Block> = None;
	for (index, line) in markdown.lines().enumerate() {
		let fence = line.trim_start().strip_prefix("```");
		match (&mut current, fence) {
			(None, Some(info)) => {
				let mut words = info.split_whitespace();
				if words.next() == Some("zing") {
					current = Some(Block {
						line: index + 1,
						mode: words.collect::<Vec<_>>().join(" "),
						code: String::new(),
					});
				} else {
					// Some other code block: skip to its end.
					current = Some(Block { line: 0, mode: String::new(), code: String::new() });
				}
			},
			(Some(_), Some("")) => {
				let block = current.take().unwrap();
				if block.line != 0 {
					blocks.push(block);
				}
			},
			(Some(block), _) => {
				block.code += line;
				block.code += "\n";
			},
			(None, None) => {},
		}
	}
	assert!(current.is_none(), "Unterminated code block in ZING.md");
	blocks
}

/// Turn a documentation example into a complete program:
///
/// - Member definitions get `include "base.zing"` (unless the block has includes of its own)
///   and, unless they define `main`, a generated `main` calling every member.
/// - Plain statements become the body of an instrument, called from a generated `main`.
/// - In `main` mode, statements become the body of `main`, with `tutorial.zing` included.
///
/// In both statement forms, `parameter` lines are moved to the top.
fn wrap_block(block: &Block) -> String {
	let code = &block.code;
	let lines: Vec<&str> = code.lines().collect();
	let has_include = lines.iter().any(|line| line.trim_start().starts_with("include "));
	let block_headers = headers(code);
	if !block_headers.is_empty() && block.mode != "main" {
		let mut src = String::new();
		if !has_include {
			src += "include \"base.zing\"\n";
		}
		src += code;
		if !block_headers.iter().any(|h| h.name == "main") {
			src += &generate_callers(&block_headers);
		}
		return src;
	}

	let (parameters, statements): (Vec<&str>, Vec<&str>) = lines.iter()
		.partition(|line| line.trim_start().starts_with("parameter "));
	let mut src = String::from("include \"base.zing\"\n");
	if block.mode == "main" {
		src += "include \"tutorial.zing\"\n";
		src += &parameters.iter().map(|line| format!("{line}\n")).collect::<String>();
		src += "global module main() -> out: stereo\n";
		src += &indent(&statements);
	} else {
		src += &parameters.iter().map(|line| format!("{line}\n")).collect::<String>();
		src += "instrument doc_snippet() -> out: stereo\n";
		src += &indent(&statements);
		src += "\nglobal module main() -> out: stereo\n\tout = 1::doc_snippet()\n";
	}
	src
}

fn expected_error(code: &str) -> Option<String> {
	code.lines()
		.find_map(|line| line.split_once("# Error:"))
		.map(|(_, message)| message.trim().to_string())
}

#[test]
fn zing_md_examples_compile() {
	let markdown = read(&repo_root().join("ZING.md"));
	let blocks = zing_blocks(&markdown);
	assert!(!blocks.is_empty(), "No ```zing blocks found in ZING.md");

	// A file that doesn't exist, in the library directory, so includes find the library.
	let path = library_dir().join("doc_example.zing");
	let mut failures = vec![];
	for block in &blocks {
		let src = wrap_block(block);
		let result = compile(&path, &src);
		let failure = match (block.mode.as_str(), result) {
			("" | "main", Ok(())) => None,
			("" | "main", Err(errors)) => Some(errors),
			("error", Ok(())) => Some("Compiled, but is marked as an error example.".to_string()),
			("error", Err(errors)) => match expected_error(&block.code) {
				Some(expected) if !errors.contains(&expected) =>
					Some(format!("Expected an error containing '{expected}', got:\n{errors}")),
				_ => None,
			},
			(mode, _) => Some(format!("Unknown block mode '{mode}': use ```zing, ```zing main or ```zing error.")),
		};
		if let Some(failure) = failure {
			failures.push(format!("ZING.md:{}:\n{}\n--- as compiled:\n{}", block.line, failure, src));
		}
	}
	assert!(failures.is_empty(), "{} of {} examples failed:\n\n{}",
		failures.len(), blocks.len(), failures.join("\n=====\n\n"));
}

#[test]
fn library_files_compile() {
	let mut failures = vec![];
	let mut count = 0;
	for entry in fs::read_dir(library_dir()).unwrap() {
		let path = entry.unwrap().path();
		if path.extension().is_none_or(|e| e != "zing") {
			continue;
		}
		count += 1;
		let src = read(&path);
		let file_headers = headers(&src);
		let result = if file_headers.iter().any(|h| h.name == "main") {
			compile(&path, &src)
		} else {
			// A library file: compile a program that includes it and calls all its members.
			let file_name = path.file_name().unwrap().to_string_lossy();
			let program = format!("include \"{file_name}\"\n{}", generate_callers(&file_headers));
			compile(&library_dir().join("doc_check.zing"), &program)
		};
		if let Err(errors) = result {
			failures.push(format!("{}:\n{}", path.display(), errors));
		}
	}
	assert!(count > 0, "No .zing files in {}", library_dir().display());
	assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

// ============================================================
// Standard library chapter
// ============================================================

/// Sections of (title, members), each member a (header line, description).
type Library = Vec<(String, Vec<(String, String)>)>;

fn library_from_source(src: &str) -> Library {
	let mut library: Library = vec![];
	let mut comment: Vec<String> = vec![];
	for line in src.lines() {
		if line.starts_with("#####") {
			library.push((line.trim_matches('#').trim().to_string(), vec![]));
			comment.clear();
		} else if let Some(text) = line.strip_prefix('#') {
			comment.push(text.trim().to_string());
		} else if let Some(header) = parse_header(line) {
			if !header.name.starts_with('_') {
				assert!(!comment.is_empty(), "base.zing: '{}' has no comment above it", header.name);
				let section = library.last_mut().expect("base.zing: member before the first section banner");
				section.1.push((header.line, comment.join(" ")));
			}
			comment.clear();
		} else {
			comment.clear();
		}
	}
	library
}

fn library_from_markdown(markdown: &str) -> Library {
	let mut library: Library = vec![];
	let chapter = markdown.lines()
		.skip_while(|line| *line != "## Standard library")
		.skip(1)
		.take_while(|line| !line.starts_with("## "));
	for line in chapter {
		if let Some(title) = line.strip_prefix("### ") {
			library.push((title.trim().to_string(), vec![]));
		} else if line.starts_with("| `") {
			let cells: Vec<&str> = line.split('|').collect();
			assert!(cells.len() == 4, "ZING.md: expected two cells in library row: {line}");
			let header = normalize_whitespace(cells[1].trim().trim_matches('`'));
			let description = normalize_whitespace(&cells[2].replace('`', ""));
			let section = library.last_mut().expect("ZING.md: library row before the first ### section");
			section.1.push((header, description));
		}
	}
	library
}

fn library_markdown(library: &Library) -> String {
	library.iter().map(|(title, members)| {
		let rows: String = members.iter()
			.map(|(header, description)| format!("| `{header}` | {description} |\n"))
			.collect();
		format!("### {title}\n\n| Member | Description |\n|---|---|\n{rows}\n")
	}).collect()
}

#[test]
fn library_chapter_matches_base_zing() {
	let source = library_from_source(&read(&library_dir().join("base.zing")));
	let documented = library_from_markdown(&read(&repo_root().join("ZING.md")));
	assert!(source == documented,
		"The 'Standard library' chapter of ZING.md doesn't match zing/base.zing. \
		Backticks in descriptions are ignored. Expected tables:\n\n{}",
		library_markdown(&source));
}
