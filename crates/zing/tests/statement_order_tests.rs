//! Statements can come in any order: each is evaluated after the statements it refers to.
//! The first argument of `cell`, `delay` and `dyndelay` is evaluated after the whole body,
//! so only references from there can close a cycle (ZING.md, "State: cell").

use zing::compiler::Compiler;

const CYCLE: &str = "A cycle must go through the first argument of a cell or delay.";

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

#[test]
fn forward_references_compile_like_dependency_order() {
	let forward = compile(r#"
		global module main() -> out: stereo
			out = [a, b + d]
			a = b + 1
			b = c * 2
			c = 5
			d = cell(d + a, 0)
	"#).unwrap();
	let backward = compile(r#"
		global module main() -> out: stereo
			c = 5
			b = c * 2
			a = b + 1
			d = cell(d + a, 0)
			out = [a, b + d]
	"#).unwrap();
	assert_eq!(forward, backward);
}

#[test]
fn forward_reference_carries_its_type() {
	// `b` is dynamic, so `a` can't be static.
	assert_error(r#"
		global module main() -> out: stereo
			a: static = b + 1
			b = cell(b + 1, 0)
			out = a
	"#, "can't be assigned to 'a': static");
}

#[test]
fn self_reference_is_a_cycle() {
	assert_error(r#"
		global module main() -> out: stereo
			out = out + 1
	"#, &format!("'out' depends on itself. {CYCLE}"));
}

#[test]
fn cycle_is_reported_along_its_path() {
	let errors = compile(r#"
		global module main() -> out: stereo
			a = b + 1
			b = c * 2
			c = a
			out = a
	"#).unwrap_err();
	for expected in [&format!("'a' depends on itself. {CYCLE}"), "'b' depends on 'c' here.", "'c' depends on 'a' here."] {
		assert!(errors.contains(expected), "Expected '{}', got:\n{}", expected, errors);
	}
}

#[test]
fn static_cycle_is_rejected() {
	assert_error(r#"
		global module main() -> out: stereo
			a: static = b
			b: static = a + 1
			out = a
	"#, CYCLE);
}

#[test]
fn cycle_in_function_is_rejected() {
	assert_error(r#"
		function f(x: mono) -> out: mono
			out = y
			y = out + x

		global module main() -> out: stereo
			out = f(1)
	"#, CYCLE);
}

#[test]
fn only_the_first_argument_of_state_modules_can_close_a_cycle() {
	assert_error(r#"
		global module main() -> out: stereo
			x = cell(y, 0) + y
			y = x + 1
			out = x
	"#, CYCLE);
	assert_error(r#"
		global module main() -> out: stereo
			out = dyndelay(t, t, 10)
			t = out + 1
	"#, CYCLE);
}

#[test]
fn first_argument_of_state_modules_can_close_a_cycle() {
	compile(r#"
		global module main() -> out: stereo
			a = cell(b + s, 0)
			d = delay(b * s, 10)
			e = dyndelay(b - s, 3, 10)
			b = a + d + e
			s: static = 2
			out = b
	"#).unwrap();
}
