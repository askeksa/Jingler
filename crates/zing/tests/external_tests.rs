use ir::{ExternalProcedure, Instruction, ProcedureKind, Scope, Type, ValueType, Width};
use zing::compiler::Compiler;

const MAIN: &str = "global module main() -> out: stereo\n\tout = 0\n";

fn compile(src: &str) -> Result<ir::Program, String> {
	Compiler::new("test.zing".to_string(), src.to_string())
		.compile()
		.map_err(|errors| errors.collect::<Vec<_>>().join("\n"))
}

fn assert_error(src: &str, expected: &str) {
	match compile(src) {
		Ok(_) => panic!("Expected error '{}', but compilation succeeded", expected),
		Err(errors) => assert!(errors.contains(expected), "Expected error '{}', got:\n{}", expected, errors),
	}
}

fn procedure<'p>(program: &'p ir::Program, name: &str, kind: ProcedureKind) -> &'p ir::Procedure {
	program.procedures.iter()
		.find(|p| p.name == name && p.kind == kind)
		.unwrap_or_else(|| panic!("No procedure '{}' of kind {}", name, kind))
}

fn main_code(program: &ir::Program, scope: Scope) -> &Vec<Instruction> {
	&procedure(program, "main", ProcedureKind::Module { scope }).code
}

fn external(name: &str, kind: ProcedureKind, inputs: Vec<Type>, outputs: Vec<Type>) -> ExternalProcedure {
	ExternalProcedure { name: name.to_string(), kind, inputs, outputs }
}

const MONO: Type = Type { width: Width::Mono, value_type: ValueType::Number };
const STEREO: Type = Type { width: Width::Stereo, value_type: ValueType::Number };
const STATIC: ProcedureKind = ProcedureKind::Module { scope: Scope::Static };
const DYNAMIC: ProcedureKind = ProcedureKind::Module { scope: Scope::Dynamic };

#[test]
fn external_function_is_listed_and_called_externally() {
	let program = compile(r#"
		external function lookup(id: mono, offset: mono) -> out: stereo

		global module main() -> out: stereo
			t = cell(t + 1, 0)
			out = lookup(t, 0.5)
	"#).unwrap();
	assert_eq!(program.externals, vec![
		external("lookup", ProcedureKind::Function, vec![MONO, MONO], vec![STEREO]),
	]);
	assert!(program.procedures.iter().all(|p| p.name != "lookup"));
	assert!(main_code(&program, Scope::Dynamic).contains(&Instruction::CallExternal(0)));
}

#[test]
fn external_module_parts_are_listed_like_procedures() {
	let program = compile(r#"
		external module osc(freq: static mono, shape: dynamic mono, tone: static stereo) -> out: stereo

		global module main() -> out: stereo
			out = osc(440, 0.5, [1, 2])
	"#).unwrap();
	assert_eq!(program.externals, vec![
		external("osc", DYNAMIC, vec![MONO], vec![STEREO]),
		external("osc", STATIC, vec![MONO, STEREO], vec![]),
	]);
	assert!(program.procedures.iter().all(|p| p.name != "osc"));
	assert!(main_code(&program, Scope::Dynamic).contains(&Instruction::CallExternal(0)));
	assert!(main_code(&program, Scope::Static).contains(&Instruction::CallExternal(1)));
}

#[test]
fn buffer_fill_calls_both_module_parts_from_the_static_procedure() {
	let program = compile(r#"
		external module ramp(start: static mono) -> out: mono

		global module main() -> out: stereo
			buf: mono buffer = for 4 buffer ramp(10)
			out = buf[0]
	"#).unwrap();
	let code = main_code(&program, Scope::Static);
	assert!(code.contains(&Instruction::CallExternal(0)));
	assert!(code.contains(&Instruction::CallExternal(1)));
}

#[test]
fn externals_are_listed_in_declaration_order() {
	let program = compile(r#"
		external function b(x: mono) -> y: mono
		external module a(x: static mono) -> y: mono

		global module main() -> out: stereo
			out = a(1) + b(2)
	"#).unwrap();
	let parts: Vec<(&str, ProcedureKind)> = program.externals.iter().map(|e| (e.name.as_str(), e.kind)).collect();
	assert_eq!(parts, vec![("b", ProcedureKind::Function), ("a", DYNAMIC), ("a", STATIC)]);
}

#[test]
fn bools_are_numbers_in_the_ir() {
	let program = compile(r#"
		external function invert(b: mono bool) -> out: mono bool

		global module main() -> out: stereo
			out = invert(1 < 2) ? 5 : 7
	"#).unwrap();
	assert_eq!(program.externals, vec![
		external("invert", ProcedureKind::Function, vec![MONO], vec![MONO]),
	]);
}

#[test]
fn unused_external_members_are_dropped() {
	let program = compile(&format!(r#"
		external function unused(x: mono) -> y: mono
		external module unused_module(x: mono) -> y: mono
		{MAIN}
	"#)).unwrap();
	assert!(program.externals.is_empty());
}

#[test]
fn external_member_is_pretty_printed() {
	let src = format!("external module osc(freq: static mono) -> out: stereo\n{MAIN}");
	let mut compiler = Compiler::new("test.zing".to_string(), src);
	compiler.compile().unwrap_or_else(|_| panic!("Compilation failed"));
	let printed = compiler.ast().unwrap().to_string();
	assert!(printed.contains("external module osc("), "{}", printed);
}

#[test]
fn external_instrument_is_rejected() {
	assert_error(&format!("external instrument beep() -> out: mono\n{MAIN}"),
		"Instruments can't be external.");
}

#[test]
fn external_context_is_rejected() {
	assert_error(&format!("external global function f(x: mono) -> y: mono\n{MAIN}"),
		"External members can't be global or note.");
	assert_error(&format!("external note module m(x: mono) -> y: mono\n{MAIN}"),
		"External members can't be global or note.");
}

#[test]
fn external_midi_inputs_are_rejected() {
	assert_error(&format!("external module ch::m(x: mono) -> y: mono\n{MAIN}"),
		"External members can't have MIDI inputs.");
}

#[test]
fn external_body_is_rejected() {
	assert_error(&format!("external function f(x: mono) -> y: mono\n\ty = x\n{MAIN}"),
		"External members can't have a body.");
}

#[test]
fn external_main_is_rejected() {
	assert_error("external global module main() -> out: stereo\n", "'main' can't be external.");
}

#[test]
fn external_generic_is_rejected() {
	assert_error(&format!("external function f(x: generic) -> y: generic\n{MAIN}"),
		"External members can't have generic inputs or outputs.");
}

#[test]
fn external_buffer_is_rejected() {
	assert_error(&format!("external function f(x: mono buffer) -> y: mono\n{MAIN}"),
		"External members can't have buffer inputs or outputs.");
}

#[test]
fn external_members_follow_the_usual_signature_rules() {
	assert_error(&format!("external function f(x: static mono) -> y: mono\n{MAIN}"),
		"Function inputs or outputs can't be marked static or dynamic.");
	assert_error(&format!("external module m(x: mono) -> y: static mono\n{MAIN}"),
		"Module outputs can't be static.");
	assert_error(&format!("external function f(x) -> y: mono\n{MAIN}"),
		"Inputs and outputs must specify explicit width.");
}

#[test]
fn external_module_follows_the_usual_call_rules() {
	assert_error(r#"
		external module m(x: mono) -> y: mono

		function f(x: mono) -> y: mono
			y = m(x)

		global module main() -> out: stereo
			out = f(1)
	"#, "Modules can't be called from functions.");
}

#[test]
fn external_name_must_be_unique() {
	assert_error(&format!("external function sin(x: mono) -> y: mono\n{MAIN}"),
		"has the same name as a built-in");
	assert_error(&format!("external function f(x: mono) -> y: mono\nfunction f(x: mono) -> y: mono\n\ty = x\n{MAIN}"),
		"Duplicate definition of 'f'.");
}
