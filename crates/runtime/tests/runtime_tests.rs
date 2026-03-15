use runtime::{JinglerRuntime, default_jingler_runtime};
use zing::compiler::Compiler;

const SAMPLE_RATE: f32 = 44100.0;

fn compile(src: &str) -> ir::Program {
	Compiler::new("test.zing".to_string(), src.to_string())
		.compile()
		.unwrap_or_else(|mut e| panic!("Compilation failed:\n{}", e.next().unwrap_or_default()))
}

fn make_runtime(src: &str) -> Box<dyn JinglerRuntime> {
	let program = compile(src);
	let mut rt = default_jingler_runtime().unwrap();
	rt.load_program(&program, SAMPLE_RATE).unwrap();
	rt
}

fn run(src: &str, n: usize) -> Vec<[f64; 2]> {
	let mut rt = make_runtime(src);
	(0..n).map(|_| rt.next_sample().unwrap()).collect()
}

fn run1(src: &str) -> [f64; 2] {
	run(src, 1)[0]
}

fn assert_approx(a: f64, b: f64) {
	assert!(
		(a - b).abs() < 1e-4,
		"expected {b}, got {a} (diff {})",
		(a - b).abs()
	);
}

fn assert_sample(s: [f64; 2], left: f64, right: f64) {
	assert_approx(s[0], left);
	assert_approx(s[1], right);
}

fn assert_mono(s: [f64; 2], value: f64) {
	assert_approx(s[0], value);
	assert_approx(s[1], value);
}

// ============================================================
// Constants & literals
// ============================================================

#[test]
fn constant_one() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0");
	assert_mono(s, 1.0);
}

#[test]
fn constant_zero() {
	let s = run1("global module main () -> (out: stereo)  out = 0.0");
	assert_mono(s, 0.0);
}

#[test]
fn constant_integer() {
	let s = run1("global module main () -> (out: stereo)  out = 42");
	assert_mono(s, 42.0);
}

#[test]
fn constant_hex() {
	let s = run1("global module main () -> (out: stereo)  out = 0x10");
	assert_mono(s, 16.0);
}

#[test]
fn constant_negative() {
	let s = run1("global module main () -> (out: stereo)  out = -3.5");
	assert_mono(s, -3.5);
}

// ============================================================
// Arithmetic
// ============================================================

#[test]
fn add() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0 + 2.0");
	assert_mono(s, 3.0);
}

#[test]
fn sub() {
	let s = run1("global module main () -> (out: stereo)  out = 5.0 - 2.0");
	assert_mono(s, 3.0);
}

#[test]
fn mul() {
	let s = run1("global module main () -> (out: stereo)  out = 2.0 * 3.0");
	assert_mono(s, 6.0);
}

#[test]
fn div() {
	let s = run1("global module main () -> (out: stereo)  out = 6.0 / 2.0");
	assert_mono(s, 3.0);
}

#[test]
fn unary_neg() {
	let s = run1("global module main () -> (out: stereo)  out = -(4.0)");
	assert_mono(s, -4.0);
}

#[test]
fn compound_expression() {
	let s = run1("global module main () -> (out: stereo)  out = (2 + 3) * 4 - 1");
	assert_mono(s, 19.0);
}

#[test]
fn addsub_stereo() {
	let src = r#"
		global module main () -> (out: stereo)
			out = [3.0, 3.0] -+ [1.0, 1.0]
	"#;
	let s = run1(src);
	assert_sample(s, 2.0, 4.0);
}

// ============================================================
// Comparisons (tested via ternary)
// ============================================================

#[test]
fn cmp_eq_true() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0 == 1.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

#[test]
fn cmp_eq_false() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0 == 2.0 ? 10.0 : 0.0");
	assert_mono(s, 0.0);
}

#[test]
fn cmp_neq() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0 != 2.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

#[test]
fn cmp_less() {
	let s = run1("global module main () -> (out: stereo)  out = 1.0 < 2.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

#[test]
fn cmp_less_eq() {
	let s = run1("global module main () -> (out: stereo)  out = 2.0 <= 2.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

#[test]
fn cmp_greater() {
	let s = run1("global module main () -> (out: stereo)  out = 3.0 > 2.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

#[test]
fn cmp_greater_eq() {
	let s = run1("global module main () -> (out: stereo)  out = 2.0 >= 2.0 ? 10.0 : 0.0");
	assert_mono(s, 10.0);
}

// ============================================================
// Logic & booleans
// ============================================================

#[test]
fn logic_and() {
	let s = run1("global module main () -> (out: stereo)  out = (true & true) ? 1.0 : 0.0");
	assert_mono(s, 1.0);
}

#[test]
fn logic_and_false() {
	let s = run1("global module main () -> (out: stereo)  out = (true & false) ? 1.0 : 0.0");
	assert_mono(s, 0.0);
}

#[test]
fn logic_or() {
	let s = run1("global module main () -> (out: stereo)  out = (false | true) ? 1.0 : 0.0");
	assert_mono(s, 1.0);
}

#[test]
fn logic_xor() {
	let s = run1("global module main () -> (out: stereo)  out = (true ^ true) ? 1.0 : 0.0");
	assert_mono(s, 0.0);
}

#[test]
fn logic_not() {
	let s = run1("global module main () -> (out: stereo)  out = (!false) ? 1.0 : 0.0");
	assert_mono(s, 1.0);
}

// ============================================================
// Rounding / math
// ============================================================

#[test]
fn floor_fn() {
	let s = run1("global module main () -> (out: stereo)  out = floor(1.7)");
	assert_mono(s, 1.0);
}

#[test]
fn ceil_fn() {
	let s = run1("global module main () -> (out: stereo)  out = ceil(1.2)");
	assert_mono(s, 2.0);
}

#[test]
fn round_fn() {
	let s = run1("global module main () -> (out: stereo)  out = round(1.4)");
	assert_mono(s, 1.0);
}

#[test]
fn trunc_fn() {
	let s = run1("global module main () -> (out: stereo)  out = trunc(1.9)");
	assert_mono(s, 1.0);
}

#[test]
fn sin_zero() {
	let s = run1("global module main () -> (out: stereo)  out = sin(0)");
	assert_mono(s, 0.0);
}

#[test]
fn cos_zero() {
	let s = run1("global module main () -> (out: stereo)  out = cos(0)");
	assert_mono(s, 1.0);
}

#[test]
fn sincos_zero() {
	let s = run1(r#"
		global module main () -> (out: stereo)
			s, c = sincos(0)
			out = [s, c]
	"#);
	assert_sample(s, 0.0, 1.0);
}

#[test]
fn tan_zero() {
	let s = run1("global module main () -> (out: stereo)  out = tan(0)");
	assert_mono(s, 0.0);
}

#[test]
fn exp2_zero() {
	let s = run1("global module main () -> (out: stereo)  out = exp2(0)");
	assert_mono(s, 1.0);
}

#[test]
fn exp2_one() {
	let s = run1("global module main () -> (out: stereo)  out = exp2(1)");
	assert_mono(s, 2.0);
}

#[test]
fn log2_one() {
	let s = run1("global module main () -> (out: stereo)  out = log2(1)");
	assert_mono(s, 0.0);
}

#[test]
fn log2_four() {
	let s = run1("global module main () -> (out: stereo)  out = log2(4)");
	assert_mono(s, 2.0);
}

#[test]
fn sqrt_four() {
	let s = run1("global module main () -> (out: stereo)  out = sqrt(4)");
	assert_mono(s, 2.0);
}

#[test]
fn pow_fn() {
	let s = run1("global module main () -> (out: stereo)  out = pow(2, 3)");
	assert_mono(s, 8.0);
}

#[test]
fn atan2_fn() {
	let s = run1("global module main () -> (out: stereo)  out = atan2(1, 1)");
	assert_approx(s[0], std::f64::consts::FRAC_PI_4);
}

#[test]
fn min_fn() {
	let s = run1("global module main () -> (out: stereo)  out = min(3, 1)");
	assert_mono(s, 1.0);
}

#[test]
fn max_fn() {
	let s = run1("global module main () -> (out: stereo)  out = max(1, 3)");
	assert_mono(s, 3.0);
}

// ============================================================
// Channel operations
// ============================================================

#[test]
fn stereo_output() {
	let s = run1("global module main () -> (out: stereo)  out = [0.5, 0.75]");
	assert_sample(s, 0.5, 0.75);
}

#[test]
fn left_fn() {
	// left() returns mono, which auto-expands to stereo for main output
	let s = run1("global module main () -> (out: stereo)  out = left([3.0, 7.0])");
	assert_mono(s, 3.0);
}

#[test]
fn right_fn() {
	let s = run1("global module main () -> (out: stereo)  out = right([3.0, 7.0])");
	assert_mono(s, 7.0);
}

#[test]
fn center_fn() {
	let s = run1("global module main () -> (out: stereo)  out = center([2.0, 4.0])");
	assert_mono(s, 3.0);
}

#[test]
fn swap_fn() {
	let s = run1("global module main () -> (out: stereo)  out = swap([2.0, 4.0])");
	assert_sample(s, 4.0, 2.0);
}

#[test]
fn mono_to_stereo_expand() {
	let src = r#"
		global module main () -> (out: stereo)
			m: mono = 5.0
			out = [0.0, 0.0] + m
	"#;
	let s = run1(src);
	assert_sample(s, 5.0, 5.0);
}

// ============================================================
// State: cell
// cell(update, init) returns current value, then stores update for next sample
// First sample returns init, second returns first update, etc.
// ============================================================

#[test]
fn cell_counter() {
	let src = r#"
		global module main () -> (out: stereo)
			out = cell(out + 1, 0)
	"#;
	let samples = run(src, 5);
	// cell returns current value before update
	// sample 0: returns init=0, stores 0+1=1
	// sample 1: returns 1, stores 1+1=2
	// sample 2: returns 2, stores 2+1=3
	for (i, s) in samples.iter().enumerate() {
		assert_mono(*s, i as f64);
	}
}

#[test]
fn cell_static_init() {
	let src = r#"
		global module main () -> (out: stereo)
			init: static mono = 100
			out = cell(out + 1, init)
	"#;
	let samples = run(src, 3);
	// sample 0: returns init=100, stores 100+1=101
	// sample 1: returns 101, stores 102
	assert_mono(samples[0], 100.0);
	assert_mono(samples[1], 101.0);
	assert_mono(samples[2], 102.0);
}

#[test]
fn cell_multiple() {
	let src = r#"
		global module main () -> (out: stereo)
			a = cell(a + 1, 0)
			b = cell(b + 10, 0)
			out = a + b
	"#;
	let samples = run(src, 3);
	// sample 0: a=0, b=0, out=0
	// sample 1: a=1, b=10, out=11
	// sample 2: a=2, b=20, out=22
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 11.0);
	assert_mono(samples[2], 22.0);
}

#[test]
fn cell_feedback() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = cell(out + t, 0)
	"#;
	let samples = run(src, 5);
	// t: 0, 1, 2, 3, 4   (cell returns current before update)
	// out: 0, 0, 1, 3, 6  (cumulative sum of t)
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 1.0);
	assert_mono(samples[3], 3.0);
	assert_mono(samples[4], 6.0);
}

// ============================================================
// State: delay
// ============================================================

#[test]
fn delay_basic() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = delay(t, 3)
	"#;
	let samples = run(src, 6);
	// t: 0, 1, 2, 3, 4, 5  (cell returns before update)
	// delay(t, 3): value from 3 samples ago, buffer init with zeros
	// sample 0: t=0, delayed=0
	// sample 1: t=1, delayed=0
	// sample 2: t=2, delayed=0
	// sample 3: t=3, delayed=0 (t from sample 0)
	// sample 4: t=4, delayed=1 (t from sample 1)
	// sample 5: t=5, delayed=2
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 0.0);
	assert_mono(samples[3], 0.0);
	assert_mono(samples[4], 1.0);
	assert_mono(samples[5], 2.0);
}

// ============================================================
// Buffers
// ============================================================

#[test]
fn buffer_literal_and_index() {
	let src = r#"
		global module main () -> (out: stereo)
			buf: mono buffer = {10.0, 20.0, 30.0}
			out = buf[0]
	"#;
	let s = run1(src);
	assert_mono(s, 10.0);
}

#[test]
fn buffer_literal_second_element() {
	let src = r#"
		global module main () -> (out: stereo)
			buf: mono buffer = {10.0, 20.0, 30.0}
			out = buf[1]
	"#;
	let s = run1(src);
	assert_mono(s, 20.0);
}

#[test]
fn buffer_length_fn() {
	let src = r#"
		global module main () -> (out: stereo)
			buf: mono buffer = {1.0, 2.0, 3.0, 4.0, 5.0}
			out = length(buf)
	"#;
	let s = run1(src);
	assert_mono(s, 5.0);
}

// ============================================================
// Conditionals
// ============================================================

#[test]
fn ternary_true_branch() {
	let s = run1("global module main () -> (out: stereo)  out = 1 > 0 ? 10.0 : 20.0");
	assert_mono(s, 10.0);
}

#[test]
fn ternary_false_branch() {
	let s = run1("global module main () -> (out: stereo)  out = 0 > 1 ? 10.0 : 20.0");
	assert_mono(s, 20.0);
}

#[test]
fn ternary_nested() {
	let s = run1("global module main () -> (out: stereo)  out = 1 > 0 ? (2 > 3 ? 10 : 20) : 30");
	assert_mono(s, 20.0);
}

// ============================================================
// For loops
// ============================================================

#[test]
fn for_loop_add() {
	let src = r#"
		global module main () -> (out: stereo)
			out = for i to 4 add i
	"#;
	let s = run1(src);
	// 0 + 1 + 2 + 3 = 6
	assert_mono(s, 6.0);
}

#[test]
fn for_loop_mul() {
	let src = r#"
		global module main () -> (out: stereo)
			out = for i to 4 mul (i + 1)
	"#;
	let s = run1(src);
	// 1 * 2 * 3 * 4 = 24
	assert_mono(s, 24.0);
}

// ============================================================
// Functions
// ============================================================

#[test]
fn function_call() {
	let src = r#"
		function double(x: mono) -> out: mono
			out = x * 2

		global module main () -> (out: stereo)
			out = double(5)
	"#;
	let s = run1(src);
	assert_mono(s, 10.0);
}

#[test]
fn function_method_syntax() {
	let src = r#"
		function double(x: mono) -> out: mono
			out = x * 2

		global module main () -> (out: stereo)
			out = 7.0.double
	"#;
	let s = run1(src);
	assert_mono(s, 14.0);
}

#[test]
fn function_multi_output() {
	let src = r#"
		function exchange(a: mono, b: mono) -> (x: mono, y: mono)
			x = b
			y = a

		global module main () -> (out: stereo)
			x, y = exchange(3, 7)
			out = x - y
	"#;
	let s = run1(src);
	assert_mono(s, 4.0);
}

// ============================================================
// Modules
// ============================================================

#[test]
fn module_with_state() {
	let src = r#"
		module counter() -> out: mono
			out = cell(out + 1, 0)

		global module main () -> (out: stereo)
			out = counter()
	"#;
	let samples = run(src, 3);
	// cell in counter: sample 0 returns init=0, stores 1
	// but the output is cell return + update already stored... let me just check
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 1.0);
	assert_mono(samples[2], 2.0);
}

#[test]
fn module_with_input() {
	let src = r#"
		module scale(x: mono, factor: static mono) -> out: mono
			out = x * factor

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = scale(t, 3)
	"#;
	let samples = run(src, 3);
	// t: 0, 1, 2 (cell returns before update)
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 3.0);
	assert_mono(samples[2], 6.0);
}

#[test]
fn nested_module_call() {
	let src = r#"
		module inner() -> out: mono
			out = cell(out + 1, 0)

		module outer() -> out: mono
			out = inner() * 2

		global module main () -> (out: stereo)
			out = outer()
	"#;
	let samples = run(src, 3);
	// inner: 0, 1, 2
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 2.0);
	assert_mono(samples[2], 4.0);
}

// ============================================================
// Instruments
// ============================================================

#[test]
fn instrument_note_on() {
	let src = r#"
		instrument beep() -> out: mono
			out = 1.0

		global module main () -> (out: stereo)
			out = 1::beep()
	"#;
	let mut rt = make_runtime(src);

	// Before note_on, instrument should be silent
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 0.0);

	rt.note_on(0, 60, 127).unwrap();

	// After note_on, should produce output
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 1.0);
}

#[test]
fn instrument_key_property() {
	let src = r#"
		instrument tone() -> out: mono
			out = key()

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 60.0);
}

#[test]
fn instrument_velocity_property() {
	let src = r#"
		instrument tone() -> out: mono
			out = velocity()

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 100).unwrap();
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 100.0);
}

#[test]
fn instrument_gate_property() {
	let src = r#"
		instrument tone() -> out: mono
			out = gate() ? 1.0 : 0.0

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 1.0);

	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 0.0);
}

// ============================================================
// Parameters
// ============================================================

#[test]
fn parameter_default() {
	let src = r#"
		parameter gain 0 to 100 = 50

		global module main () -> (out: stereo)
			out = gain
	"#;
	let s = run1(src);
	// Parameter defaults to 0 in the constant pool (set_parameter not called)
	assert_approx(s[0], 0.0);
}

#[test]
fn parameter_set() {
	let src = r#"
		parameter gain 0 to 100

		global module main () -> (out: stereo)
			out = gain
	"#;
	let mut rt = make_runtime(src);

	// Set parameter to 0.5 normalized → value = 0 + 0.5 * (100-0) = 50
	rt.set_parameter(0, 0.5).unwrap();
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 50.0);
}

// ============================================================
// Sample rate
// ============================================================

#[test]
fn samplerate_fn() {
	let s = run1("global module main () -> (out: stereo)  out = samplerate()");
	assert_mono(s, SAMPLE_RATE as f64);
}

// ============================================================
// Random
// ============================================================

#[test]
fn random_deterministic() {
	let src = "global module main () -> (out: stereo)  out = random(1, 1)";
	let s1 = run1(src);
	let s2 = run1(src);
	assert_eq!(s1[0], s2[0]);
}

#[test]
fn random_different_seeds() {
	let s1 = run1("global module main () -> (out: stereo)  out = random(1, 1)");
	let s2 = run1("global module main () -> (out: stereo)  out = random(1, 2)");
	assert!(s1[0] != s2[0], "Expected different values for different seeds");
}

#[test]
fn random_specific_outputs() {
	// Verify exact output for various input combinations.
	// The random function is a deterministic PRNG seeded by its two arguments.
	let cases: &[(i64, i64, f64)] = &[
		(    0,     0,  0.0),
		(    1,     1, -0.6668139002285898),
		(    1,     2, -0.6345644812099636),
		(    2,     1,  0.7871766686439514),
		(    0,     1, -0.6515078167431056),
		(    1,     0, -0.5959545937366784),
		(   10,     5, -0.007433788850903511),
		(  100,   200, -0.09859279682859778),
		(   -1,     1, -0.05100200977176428),
		(    1,    -1, -0.2551103332079947),
		(   -3,    -7,  0.18342527095228434),
		(   42,    99, -0.4728339253924787),
		( 1000,  1000,  0.9038781966082752),
	];
	for &(a, b, expected) in cases {
		let src = format!("global module main () -> (out: stereo)  out = random({a}, {b})");
		let s = run1(&src);
		assert_eq!(s[0], expected, "random({a}, {b}) left mismatch");
		assert_eq!(s[1], expected, "random({a}, {b}) right mismatch");
	}
}

// ============================================================
// Complex programs
// ============================================================

#[test]
fn abs_function() {
	let src = r#"
		function abs(a: generic) -> b: generic
			b = max(a, -a)

		global module main () -> (out: stereo)
			out = abs(-7)
	"#;
	let s = run1(src);
	assert_mono(s, 7.0);
}

#[test]
fn clamp_function() {
	let src = r#"
		function clamp(x: generic, min_: generic, max_: generic) -> out: generic
			out = min(max(x, min_), max_)

		global module main () -> (out: stereo)
			out = clamp(15, 0, 10)
	"#;
	let s = run1(src);
	assert_mono(s, 10.0);
}

#[test]
fn phase_module() {
	let src = r#"
		function mod1(a: generic) -> b: generic
			b = a - floor(a)

		module phase(freq: generic, offset: static generic) -> out: generic
			out = mod1(cell(out + freq, offset))

		global module main () -> (out: stereo)
			out = phase(0.25, 0)
	"#;
	let samples = run(src, 5);
	// cell returns current before update:
	// sample 0: cell returns init=0, mod1(0)=0, stores 0+0.25=0.25
	// sample 1: returns 0.25, mod1(0.25)=0.25, stores 0.25+0.25=0.5
	// sample 2: returns 0.5, mod1(0.5)=0.5
	// sample 3: returns 0.75, mod1(0.75)=0.75
	// sample 4: returns 1.0, mod1(1.0)=0.0
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.25);
	assert_mono(samples[2], 0.5);
	assert_mono(samples[3], 0.75);
	assert_mono(samples[4], 0.0);
}

#[test]
fn stereo_cell() {
	let src = r#"
		global module main () -> (out: stereo)
			out = cell(out + [1.0, 2.0], [0.0, 0.0])
	"#;
	let samples = run(src, 3);
	// cell returns current value before update:
	// sample 0: returns [0,0], stores [0+1, 0+2]=[1,2]
	// sample 1: returns [1,2], stores [1+1, 2+2]=[2,4]
	// sample 2: returns [2,4], stores [3,6]
	assert_sample(samples[0], 0.0, 0.0);
	assert_sample(samples[1], 1.0, 2.0);
	assert_sample(samples[2], 2.0, 4.0);
}

// ============================================================
// State: dyndelay
// ============================================================

#[test]
fn dyndelay_fixed_delay() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = dyndelay(t, 2, 4)
	"#;
	let samples = run(src, 6);
	// t: 0, 1, 2, 3, 4, 5  (cell returns before update)
	// dyndelay with constant delay=2, maxdelay=4:
	// same as delay(t, 2) — value from 2 samples ago
	// sample 0: delayed=0 (buffer init zero)
	// sample 1: delayed=0
	// sample 2: delayed=0 (t from sample 0)
	// sample 3: delayed=1 (t from sample 1)
	// sample 4: delayed=2
	// sample 5: delayed=3
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 0.0);
	assert_mono(samples[3], 1.0);
	assert_mono(samples[4], 2.0);
	assert_mono(samples[5], 3.0);
}

#[test]
fn dyndelay_max_delay() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = dyndelay(t, 4, 4)
	"#;
	let samples = run(src, 8);
	// t: 0, 1, 2, 3, 4, 5, 6, 7
	// delay=4 (equal to maxdelay): value from 4 samples ago
	// samples 0-3: 0 (buffer init zero)
	// sample 4: 0 (t from sample 0)
	// sample 5: 1
	// sample 6: 2
	// sample 7: 3
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 0.0);
	assert_mono(samples[3], 0.0);
	assert_mono(samples[4], 0.0);
	assert_mono(samples[5], 1.0);
	assert_mono(samples[6], 2.0);
	assert_mono(samples[7], 3.0);
}

#[test]
fn dyndelay_varying_delay() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 1)
			d = cell(d + 1, 1)
			out = dyndelay(t, d, 8)
	"#;
	let samples = run(src, 8);
	// t: 1, 2, 3, 4, 5, 6, 7, 8  (cell returns before update)
	// d: 1, 2, 3, 4, 5, 6, 7, 8  (delay amount increases each sample)
	// Since d == t, we always look back exactly as far as we've progressed,
	// so we always read the initial zero.
	for i in 0..8 {
		assert_mono(samples[i], 0.0);
	}
}

#[test]
fn dyndelay_delay_one() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = dyndelay(t, 1, 4)
	"#;
	let samples = run(src, 5);
	// t: 0, 1, 2, 3, 4
	// delay=1: value from 1 sample ago
	// sample 0: 0 (buffer zero)
	// sample 1: 0 (t from sample 0)
	// sample 2: 1
	// sample 3: 2
	// sample 4: 3
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 1.0);
	assert_mono(samples[3], 2.0);
	assert_mono(samples[4], 3.0);
}

#[test]
fn dyndelay_fractional_rounds_down() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = dyndelay(t, 2.3, 8)
	"#;
	let samples = run(src, 6);
	// cvtsd2si rounds 2.3 to 2 (round-to-nearest-even)
	// Same as delay=2: value from 2 samples ago
	// t: 0, 1, 2, 3, 4, 5
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 0.0);
	assert_mono(samples[3], 1.0);
	assert_mono(samples[4], 2.0);
	assert_mono(samples[5], 3.0);
}

#[test]
fn dyndelay_fractional_rounds_up() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = dyndelay(t, 2.7, 8)
	"#;
	let samples = run(src, 7);
	// cvtsd2si rounds 2.7 to 3 (round-to-nearest-even)
	// Same as delay=3: value from 3 samples ago
	// t: 0, 1, 2, 3, 4, 5, 6
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 0.0);
	assert_mono(samples[3], 0.0);
	assert_mono(samples[4], 1.0);
	assert_mono(samples[5], 2.0);
	assert_mono(samples[6], 3.0);
}

#[test]
fn dyndelay_step_change() {
	let src = r#"
		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			step = t < 4 ? 1 : 3
			out = dyndelay(t, step, 8)
	"#;
	let samples = run(src, 8);
	// t:    0, 1, 2, 3, 4, 5, 6, 7
	// step: 1, 1, 1, 1, 3, 3, 3, 3  (switches from delay=1 to delay=3 at t=4)
	// sample 0: delay=1, read t[-1]=0 (zero init)
	// sample 1: delay=1, read t[0]=0
	// sample 2: delay=1, read t[1]=1
	// sample 3: delay=1, read t[2]=2
	// sample 4: delay=3, read t[1]=1
	// sample 5: delay=3, read t[2]=2
	// sample 6: delay=3, read t[3]=3
	// sample 7: delay=3, read t[4]=4
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 0.0);
	assert_mono(samples[2], 1.0);
	assert_mono(samples[3], 2.0);
	assert_mono(samples[4], 1.0);
	assert_mono(samples[5], 2.0);
	assert_mono(samples[6], 3.0);
	assert_mono(samples[7], 4.0);
}

#[test]
fn for_loop_with_module() {
	let src = r#"
		module counter() -> out: mono
			out = cell(out + 1, 0)

		global module main () -> (out: stereo)
			out = for i to 3 add counter()
	"#;
	let samples = run(src, 3);
	// Each iteration has its own counter state
	// counter returns cell value (0 on first call, 1 on second, etc.)
	// Sample 0: 0+0+0 = 0
	// Sample 1: 1+1+1 = 3
	// Sample 2: 2+2+2 = 6
	assert_mono(samples[0], 0.0);
	assert_mono(samples[1], 3.0);
	assert_mono(samples[2], 6.0);
}
