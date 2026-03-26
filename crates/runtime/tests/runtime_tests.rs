use runtime::{JinglerRuntimeInstance, default_jingler_runtime};
use zing::compiler::Compiler;

const SAMPLE_RATE: f32 = 44100.0;

fn compile(src: &str) -> ir::Program {
	Compiler::new("test.zing".to_string(), src.to_string())
		.compile()
		.unwrap_or_else(|mut e| panic!("Compilation failed:\n{}", e.next().unwrap_or_default()))
}

fn make_runtime(src: &str) -> Box<dyn JinglerRuntimeInstance> {
	let program = compile(src);
	let rt = default_jingler_runtime().unwrap();
	let mut instance = rt.load_program(&program).unwrap();
	instance.initialize(SAMPLE_RATE).unwrap();
	instance
}

fn run(src: &str, n: usize) -> Vec<[f64; 2]> {
	let mut instance = make_runtime(src);
	(0..n).map(|_| instance.next_sample().unwrap()).collect()
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
// Buffer initialization loops
// ============================================================

#[test]
fn buffer_init_length() {
	let src = r#"
		module gen() -> out: mono
			out = 1.0

		global module main () -> (out: stereo)
			buf: mono buffer = for 5 buffer gen()
			out = length(buf)
	"#;
	let s = run1(src);
	assert_mono(s, 5.0);
}

#[test]
fn buffer_init_constant_body() {
	let src = r#"
		module gen() -> out: mono
			out = 7.0

		global module main () -> (out: stereo)
			buf: mono buffer = for 3 buffer gen()
			out = buf[0] + buf[1] + buf[2]
	"#;
	let s = run1(src);
	// All three elements should be 7.0
	assert_mono(s, 21.0);
}

#[test]
fn buffer_init_stateful_body() {
	let src = r#"
		module counter() -> out: mono
			out = cell(out + 1.0, 0.0)

		global module main () -> (out: stereo)
			buf: mono buffer = for 4 buffer counter()
			out = buf[0] + buf[1] * 10 + buf[2] * 100 + buf[3] * 1000
	"#;
	let s = run1(src);
	// counter produces 0, 1, 2, 3 on first sample
	// 0 + 10 + 200 + 3000 = 3210
	assert_mono(s, 3210.0);
}

#[test]
fn buffer_init_with_static_arg() {
	let src = r#"
		module gen(base: static mono) -> out: mono
			out = base * 2.0

		global module main () -> (out: stereo)
			buf: mono buffer = for 3 buffer gen(5.0)
			out = buf[0] + buf[1] + buf[2]
	"#;
	let s = run1(src);
	// gen(5) always produces 10.0, so all three elements are 10.0
	assert_mono(s, 30.0);
}

#[test]
fn buffer_init_iterate_with_for_loop() {
	let src = r#"
		module gen() -> out: mono
			out = cell(out + 1.0, 0.0)

		global module main () -> (out: stereo)
			buf: mono buffer = for 5 buffer gen()
			out = for i to 5 add buf[i]
	"#;
	let s = run1(src);
	// buf = [0, 1, 2, 3, 4], sum = 10
	assert_mono(s, 10.0);
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

		instrument static_tone() -> out: mono
			temp: static = key()
			out = temp

		global module main () -> (out: stereo)
			out = [1::tone(), 2::static_tone()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 60.0, 72.0);
}

#[test]
fn instrument_velocity_property() {
	let src = r#"
		instrument tone() -> out: mono
			out = velocity()

		instrument static_tone() -> out: mono
			temp: static = velocity()
			out = temp

		global module main () -> (out: stereo)
			out = [1::tone(), 2::static_tone()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 100).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 100.0, 127.0);
}

#[test]
fn instrument_gate_property() {
	let src = r#"
		instrument tone() -> out: mono
			out = gate() ? 1.0 : 0.0

		instrument static_tone() -> out: mono
			temp: static = gate()
			out = temp ? 1.0 : 0.0

		global module main () -> (out: stereo)
			out = [1::tone(), 2::static_tone()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 1.0);

	rt.note_off(0, 60).unwrap();
	rt.note_off(1, 72).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 1.0);
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

// ============================================================
// GM DLS (General MIDI Downloadable Sounds)
// ============================================================

#[test]
fn gmdls_specific_outputs() {
	// Verify exact output for various sound IDs and sample indices.
	// The gmdls function looks up samples from the gm.dls sound bank.
	// First argument is sound ID (0-494), second is sample index.
	let cases: &[(i64, i64, f64)] = &[
		// Sound 0 (looped): Acoustic Grand Piano
		(    0,        0,  0.0),
		(    0,        1, -0.004241943359375),
		(    0,        2, -0.015106265898793936),
		(    0,       10, -0.44199201138690114),
		(    0,      100, -0.7043263777159154),
		(    0,     1000, -0.8835582514293492),
		(    0,    10000, -0.2870198437012732),
		// Sound 1 (looped)
		(    1,        0,  0.0),
		(    1,       50, -0.8096446059644222),
		(    1,      500,  0.14663843624293804),
		// Sound 494 (looped): last sound
		(  494,        0,  0.0),
		(  494,       50, -0.4717072690837085),
		(  494,      500,  0.060060085728764534),
		// Sound 17 (unlooped): starts with nonzero, decays to zero
		(   17,        0, -0.010986328125),
		(   17,       50, -0.08834809437394142),
		(   17,      500,  0.27740538911893964),
		// Sound 120 (unlooped)
		(  120,        0,  0.0),
		(  120,       50,  0.00823981873691082),
		(  120,      500,  0.3287384216673672),
		// Sound 474 (unlooped)
		(  474,        0,  0.007110595703125),
		(  474,       50, -0.6069420916028321),
		(  474,      500, -0.6055092359893024),
	];
	for &(sound, index, expected) in cases {
		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, {index})");
		let s = run1(&src);
		assert_eq!(s[0], expected, "gmdls({sound}, {index}) left mismatch");
		assert_eq!(s[1], expected, "gmdls({sound}, {index}) right mismatch");
	}
}

#[test]
fn gmdls_looped_sound_continues_at_large_index() {
	// Looped sounds should continue producing non-zero samples
	// even at very large indices (millions of samples).
	let looped_sounds = [0, 1, 100, 200, 494];
	for sound in looped_sounds {
		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, 5000000)");
		let s = run1(&src);
		assert_ne!(s[0], 0.0, "looped sound {sound} should be non-zero at index 5000000");

		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, 10000000)");
		let s = run1(&src);
		assert_ne!(s[0], 0.0, "looped sound {sound} should be non-zero at index 10000000");
	}
}

#[test]
fn gmdls_unlooped_sound_decays_to_zero() {
	// Unlooped sounds should return zero once the sample data is exhausted.
	let unlooped_sounds = [17, 120, 474];
	for sound in unlooped_sounds {
		// At moderate index, the sound has ended
		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, 5000)");
		let s = run1(&src);
		assert_eq!(s[0], 0.0, "unlooped sound {sound} should be zero at index 5000");

		// At very large index, definitely zero
		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, 5000000)");
		let s = run1(&src);
		assert_eq!(s[0], 0.0, "unlooped sound {sound} should be zero at index 5000000");
	}
}

#[test]
fn gmdls_all_sounds_start() {
	// All 495 sounds should be accessible and produce a sample at index 0.
	// Step by 13 to save time but still test both ends of the range.
	// (The value may or may not be zero at index 0, but it should not crash.)
	for sound in (0..495).step_by(13) {
		let src = format!("global module main () -> (out: stereo)  out = gmdls({sound}, 0)");
		let s = run1(&src);
		assert_eq!(s[0], s[1], "gmdls({sound}, 0) should be mono (left == right)");
	}
}

// ============================================================
// Instruments: channel routing
// ============================================================

#[test]
fn different_instruments_different_channels_note_on() {
	// Two instruments on different channels. note_on on one channel
	// should only activate that channel's instrument.
	let src = r#"
		instrument beep() -> out: mono
			out = 1.0

		instrument boop() -> out: mono
			out = 2.0

		global module main () -> (out: stereo)
			out = [1::beep(), 2::boop()]
	"#;
	let mut rt = make_runtime(src);

	// Before any notes, both silent
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 0.0);

	// note_on channel 0 → beep activates, boop stays silent
	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 0.0);

	// note_on channel 1 → boop also activates
	rt.note_on(1, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 2.0);
}

#[test]
fn different_instruments_different_channels_note_off() {
	// note_off on one channel should only silence that channel's instrument.
	let src = r#"
		instrument beep() -> out: mono
			out = gate() ? 1.0 : 0.0

		instrument boop() -> out: mono
			out = gate() ? 2.0 : 0.0

		global module main () -> (out: stereo)
			out = [1::beep(), 2::boop()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 2.0);

	// note_off channel 0 → beep silenced, boop still playing
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 2.0);

	// note_off channel 1 → both silent
	rt.note_off(1, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 0.0);
}

#[test]
fn same_instrument_different_channels_different_processing() {
	// Same instrument type on two channels, processed differently in main.
	// Left channel gets the instrument output, right gets it doubled.
	let src = r#"
		instrument tone() -> out: mono
			out = key()

		global module main () -> (out: stereo)
			a = 1::tone()
			b = 2::tone()
			out = [a, b * 2]
	"#;
	let mut rt = make_runtime(src);

	// Channel 0 plays note 60, channel 1 plays note 72
	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// Left = key of ch0 = 60, Right = key of ch1 * 2 = 144
	assert_sample(s, 60.0, 144.0);
}

#[test]
fn different_instruments_same_channel_note_on() {
	// Two instruments on the same MIDI channel.
	// note_on should activate both.
	let src = r#"
		instrument beep() -> out: mono
			out = 1.0

		instrument boop() -> out: mono
			out = 2.0

		global module main () -> (out: stereo)
			out = [1::beep(), 1::boop()]
	"#;
	let mut rt = make_runtime(src);

	// Before note_on, both silent
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 0.0);

	// note_on channel 0 → both beep and boop activate
	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 2.0);
}

#[test]
fn different_instruments_same_channel_note_off() {
	// Two instruments on the same channel. note_off affects both.
	let src = r#"
		instrument beep() -> out: mono
			out = gate() ? 1.0 : 0.0

		instrument boop() -> out: mono
			out = gate() ? 2.0 : 0.0

		global module main () -> (out: stereo)
			out = [1::beep(), 1::boop()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 1.0, 2.0);

	// note_off on channel 0 silences both
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 0.0);
}

// ============================================================
// Instruments: multiple simultaneous notes
// ============================================================

#[test]
fn multiple_notes_different_instruments() {
	// Different notes on different instruments play simultaneously.
	let src = r#"
		instrument beep() -> out: mono
			out = key()

		instrument boop() -> out: mono
			out = key()

		global module main () -> (out: stereo)
			out = [1::beep(), 2::boop()]
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 60.0, 72.0);

	// Add another note on channel 0 — both notes should sound
	rt.note_on(0, 48, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// beep sums both notes: 60 + 48 = 108
	assert_sample(s, 108.0, 72.0);
}

#[test]
fn multiple_notes_same_instrument() {
	// Multiple notes on the same instrument sum their outputs.
	let src = r#"
		instrument tone() -> out: mono
			out = key()

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 60.0);

	// Second note on same instrument
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// Both notes playing: 60 + 72 = 132
	assert_mono(s, 132.0);

	// Third note
	rt.note_on(0, 48, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// All three: 60 + 72 + 48 = 180
	assert_mono(s, 180.0);
}

// ============================================================
// Instruments: note start/stop ordering
// ============================================================

#[test]
fn notes_start_stop_same_order() {
	// Start two notes, stop them in the same order.
	let src = r#"
		instrument tone() -> out: mono
			out = gate() ? key() : 0.0

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	// Start note 60, then note 72
	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 132.0); // 60 + 72

	// Stop note 60 first (same order as started)
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 72.0); // only 72 remains

	// Stop note 72
	rt.note_off(0, 72).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 0.0);
}

#[test]
fn notes_start_stop_different_order() {
	// Start two notes, stop them in the reverse order.
	let src = r#"
		instrument tone() -> out: mono
			out = gate() ? key() : 0.0

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	// Start note 60, then note 72
	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 132.0); // 60 + 72

	// Stop note 72 first (reverse order)
	rt.note_off(0, 72).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 60.0); // only 60 remains

	// Stop note 60
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 0.0);
}

#[test]
fn notes_start_simultaneously_stop_one() {
	// Two notes start, one stops while the other continues.
	let src = r#"
		instrument beep() -> out: mono
			out = gate() ? key() : 0.0

		instrument boop() -> out: mono
			out = gate() ? key() : 0.0

		global module main () -> (out: stereo)
			out = [1::beep(), 2::boop()]
	"#;
	let mut rt = make_runtime(src);

	// Both instruments start at the same time
	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(1, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 60.0, 72.0);

	// Stop only channel 0
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_sample(s, 0.0, 72.0);
}

#[test]
fn notes_interleaved_start_stop() {
	// Interleaved pattern: start A, start B, stop A, start C, stop B, stop C
	let src = r#"
		instrument tone() -> out: mono
			out = gate() ? key() : 0.0

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	// Start A (note 60)
	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 60.0);

	// Start B (note 72)
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 132.0); // 60 + 72

	// Stop A (note 60)
	rt.note_off(0, 60).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 72.0);

	// Start C (note 48)
	rt.note_on(0, 48, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 120.0); // 72 + 48

	// Stop B (note 72)
	rt.note_off(0, 72).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 48.0);

	// Stop C (note 48)
	rt.note_off(0, 48).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 0.0);
}

// ============================================================
// Instruments: autokill
// ============================================================

#[test]
fn autokill_silences_tiny_output() {
	// An instrument that produces a very small constant output (below the
	// autokill threshold of ~0.00006) should be killed within 10000 samples
	// and start returning exactly 0.
	let src = r#"
		instrument quiet() -> out: mono
			out = 0.00001

		global module main () -> (out: stereo)
			out = 1::quiet()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();

	// Initially the instrument should produce output
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], 0.00001);

	// Run up to 10000 samples — autokill should trigger before that
	for _ in 0..9999 {
		rt.next_sample().unwrap();
	}

	// After 10000 total samples, the note should have been killed
	let s = rt.next_sample().unwrap();
	assert_eq!(s[0], 0.0, "autokill should have silenced the note");
	assert_eq!(s[1], 0.0, "autokill should have silenced the note");
}

#[test]
fn autokill_does_not_kill_loud_output() {
	// An instrument that produces output above the autokill threshold
	// should NOT be killed.
	let src = r#"
		instrument tone() -> out: mono
			out = 1.0

		global module main () -> (out: stereo)
			out = 1::tone()
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();

	// Run 10000 samples — should still be producing output
	for _ in 0..10000 {
		rt.next_sample().unwrap();
	}

	let s = rt.next_sample().unwrap();
	assert_mono(s, 1.0);
}

// ============================================================
// Instruments: inputs
// ============================================================

#[test]
fn instrument_static_input() {
	// A static input is captured when the note is triggered.
	let src = r#"
		instrument tone(level: static mono) -> out: mono
			out = level

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = 1::tone(t)
	"#;
	let mut rt = make_runtime(src);

	// Run a few samples so t advances
	for _ in 0..5 {
		rt.next_sample().unwrap();
	}
	// t is now 5 (cell returns value before update, so after 5 calls t=4 returned, t stored=5)
	// Actually: cell(t+1, 0) returns 0,1,2,3,4 for first 5 calls.
	// After 5 next_sample calls, t will return 5 on the next call.

	// Trigger note — static input captures t at this moment
	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// t is now 5, so the static input captured 5
	let captured = s[0];
	assert_approx(captured, 5.0);

	// On subsequent samples, the static input stays at 5 even though t keeps changing
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], captured);
	let s = rt.next_sample().unwrap();
	assert_approx(s[0], captured);
}

#[test]
fn instrument_dynamic_input() {
	// A dynamic input is re-read every sample.
	let src = r#"
		instrument tone(level: mono) -> out: mono
			out = level

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = 1::tone(t)
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	let s1 = rt.next_sample().unwrap();
	let s2 = rt.next_sample().unwrap();
	let s3 = rt.next_sample().unwrap();
	// t returns 0, 1, 2 on successive samples, and the dynamic input follows
	assert_approx(s1[0], 0.0);
	assert_approx(s2[0], 1.0);
	assert_approx(s3[0], 2.0);
}

#[test]
fn instrument_static_input_same_value_for_simultaneous_notes() {
	// When two notes are triggered at the same time, both should
	// capture the same value for a static input.
	let src = r#"
		instrument tone(level: static mono) -> out: mono
			out = level * key()

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = 1::tone(t)
	"#;
	let mut rt = make_runtime(src);

	// Advance t
	for _ in 0..3 {
		rt.next_sample().unwrap();
	}

	// Trigger two notes simultaneously — both should capture the same t
	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	// t is 3. Note 60 outputs 3*60=180, note 72 outputs 3*72=216.
	// Sum = 396.
	assert_mono(s, 3.0 * 60.0 + 3.0 * 72.0);
}

#[test]
fn instrument_dynamic_input_same_value_for_simultaneous_notes() {
	// When two notes play at the same time, both should see the same
	// dynamic input value each sample.
	let src = r#"
		instrument tone(level: mono) -> out: mono
			out = level * key()

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = 1::tone(t)
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	rt.note_on(0, 72, 127).unwrap();

	let s1 = rt.next_sample().unwrap();
	let s2 = rt.next_sample().unwrap();
	// Sample 1: t=0, sum = 0*60 + 0*72 = 0
	assert_mono(s1, 0.0);
	// Sample 2: t=1, sum = 1*60 + 1*72 = 132
	assert_mono(s2, 132.0);
}

#[test]
fn instrument_multiple_notes_sum_with_inputs() {
	// Outputs from multiple notes are summed even when there are inputs.
	let src = r#"
		instrument tone(gain: mono) -> out: mono
			out = gain

		global module main () -> (out: stereo)
			out = 1::tone(5)
	"#;
	let mut rt = make_runtime(src);

	// One note: output = 5
	rt.note_on(0, 60, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 5.0);

	// Two notes: output = 5 + 5 = 10
	rt.note_on(0, 72, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 10.0);

	// Three notes: output = 5 + 5 + 5 = 15
	rt.note_on(0, 48, 127).unwrap();
	let s = rt.next_sample().unwrap();
	assert_mono(s, 15.0);
}

#[test]
fn instrument_mixed_static_and_dynamic_inputs() {
	// An instrument with both static and dynamic inputs.
	// The static input is captured once, the dynamic input changes every sample.
	let src = r#"
		instrument tone(base: static mono, modulation: mono) -> out: mono
			out = base + modulation

		global module main () -> (out: stereo)
			t = cell(t + 1, 0)
			out = 1::tone(100, t)
	"#;
	let mut rt = make_runtime(src);

	rt.note_on(0, 60, 127).unwrap();
	// t: 0, 1, 2, ...
	// base captured as 0 (t at moment of note_on processing)
	let s1 = rt.next_sample().unwrap();
	let base = s1[0]; // base + t where t=0 on first sample

	let s2 = rt.next_sample().unwrap();
	let s3 = rt.next_sample().unwrap();
	// base stays the same, modulation increases by 1 each sample
	assert_approx(s2[0], base + 1.0);
	assert_approx(s3[0], base + 2.0);
}

// ============================================================
// Multiple instances and re-initialization
// ============================================================

#[test]
fn multiple_instances_from_same_runtime() {
	let program = compile("global module main () -> (out: stereo)  out = 1.0");
	let rt = default_jingler_runtime().unwrap();

	let mut inst1 = rt.load_program(&program).unwrap();
	inst1.initialize(SAMPLE_RATE).unwrap();

	let mut inst2 = rt.load_program(&program).unwrap();
	inst2.initialize(SAMPLE_RATE).unwrap();

	let s1 = inst1.next_sample().unwrap();
	let s2 = inst2.next_sample().unwrap();
	assert_mono(s1, 1.0);
	assert_mono(s2, 1.0);
}

#[test]
fn multiple_instances_independent_state() {
	let src = r#"
		global module main () -> (out: stereo)
			out = cell(out + 1, 0)
	"#;
	let program = compile(src);
	let rt = default_jingler_runtime().unwrap();

	let mut inst1 = rt.load_program(&program).unwrap();
	inst1.initialize(SAMPLE_RATE).unwrap();

	let mut inst2 = rt.load_program(&program).unwrap();
	inst2.initialize(SAMPLE_RATE).unwrap();

	// Advance inst1 by 3 samples
	inst1.next_sample().unwrap();
	inst1.next_sample().unwrap();
	let s1 = inst1.next_sample().unwrap();
	assert_mono(s1, 2.0);

	// inst2 should still be at sample 0
	let s2 = inst2.next_sample().unwrap();
	assert_mono(s2, 0.0);
}

#[test]
fn reinitialize_resets_state() {
	let src = r#"
		global module main () -> (out: stereo)
			out = cell(out + 1, 0)
	"#;
	let mut inst = make_runtime(src);

	// Advance by 5 samples
	for _ in 0..5 {
		inst.next_sample().unwrap();
	}
	let s = inst.next_sample().unwrap();
	assert_mono(s, 5.0);

	// Re-initialize should reset state
	inst.initialize(SAMPLE_RATE).unwrap();
	let s = inst.next_sample().unwrap();
	assert_mono(s, 0.0);
}

#[test]
fn reinitialize_kills_notes() {
	let src = r#"
		instrument tone(gain: mono) -> out: mono
			out = gain

		global module main () -> (out: stereo)
			out = 1::tone(1)
	"#;
	let mut inst = make_runtime(src);

	// Play instrument
	inst.note_on(0, 60, 127).unwrap();
	let s = inst.next_sample().unwrap();
	assert_mono(s, 1.0);

	// Re-initialize should kill notes
	inst.initialize(SAMPLE_RATE).unwrap();
	let s = inst.next_sample().unwrap();
	assert_mono(s, 0.0);
}

#[test]
fn reinitialize_with_different_sample_rate() {
	let src = "global module main () -> (out: stereo)  out = samplerate()";
	let program = compile(src);
	let rt = default_jingler_runtime().unwrap();
	let mut inst = rt.load_program(&program).unwrap();

	inst.initialize(44100.0).unwrap();
	let s = inst.next_sample().unwrap();
	assert_mono(s, 44100.0);

	inst.initialize(48000.0).unwrap();
	let s = inst.next_sample().unwrap();
	assert_mono(s, 48000.0);
}
