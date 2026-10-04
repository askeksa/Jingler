use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use runtime::{
	ExternalFunction, ExternalModule, Externals, FALSE, JinglerRuntime, JinglerRuntimeHandle,
	SubmitOutcome, TRUE, compile_wasm, default_jingler_runtime, from_fn,
};
use zing::compiler::Compiler;

const SAMPLE_RATE: f32 = 44100.0;

fn compile(src: &str) -> ir::Program {
	Compiler::new("test.zing".to_string(), src.to_string())
		.compile()
		.unwrap_or_else(|mut e| panic!("Compilation failed:\n{}", e.next().unwrap_or_default()))
}

fn start(externals: Externals, src: &str) -> (Arc<dyn JinglerRuntime>, Box<dyn JinglerRuntimeHandle>) {
	let (rt, mut handle) = default_jingler_runtime(externals).unwrap();
	rt.submit_program(&compile(src)).unwrap();
	handle.initialize(SAMPLE_RATE).unwrap();
	handle.poll_pending().unwrap();
	(rt, handle)
}

fn samples(handle: &mut Box<dyn JinglerRuntimeHandle>, n: usize) -> Vec<[f64; 2]> {
	(0..n).map(|_| handle.next_sample().unwrap()).collect()
}

fn mono(values: &[f64]) -> Vec<[f64; 2]> {
	values.iter().map(|&v| [v, v]).collect()
}

/// Counts up from its static start value in steps given by its dynamic input,
/// keeping track of how many states are alive.
struct Counter {
	live_states: Arc<AtomicUsize>,
}

struct CounterState {
	value: f64,
	live_states: Arc<AtomicUsize>,
}

impl Drop for CounterState {
	fn drop(&mut self) {
		self.live_states.fetch_sub(1, Ordering::SeqCst);
	}
}

impl ExternalModule for Counter {
	type StaticInputs = f64; // start
	type DynamicInputs = f64; // step
	type Outputs = f64;
	type State = CounterState;

	fn init(&mut self, start: f64) -> CounterState {
		self.live_states.fetch_add(1, Ordering::SeqCst);
		CounterState { value: start, live_states: self.live_states.clone() }
	}

	fn process(&mut self, state: &mut CounterState, step: f64) -> f64 {
		let value = state.value;
		state.value += step;
		value
	}
}

const COUNTER: &str = "external module counter(start: static mono, step: mono) -> out: mono\n";

fn counter() -> (Counter, Arc<AtomicUsize>) {
	let live_states = Arc::new(AtomicUsize::new(0));
	(Counter { live_states: live_states.clone() }, live_states)
}

// ============================================================
// External functions
// ============================================================

#[test]
fn function_receives_and_returns_values() {
	let externals = Externals::new()
		.function("lookup", from_fn(|(id, offset): (f64, f64)| [id + offset, id - offset]));
	let (_rt, mut handle) = start(externals, r#"
		external function lookup(id: mono, offset: mono) -> out: stereo

		global module main() -> out: stereo
			out = lookup(3, 0.5)
	"#);
	assert_eq!(samples(&mut handle, 2), vec![[3.5, 2.5], [3.5, 2.5]]);
}

#[test]
fn function_with_several_outputs() {
	let externals = Externals::new()
		.function("split", from_fn(|[left, right]: [f64; 2]| (left * 10.0, [right, -right])));
	let (_rt, mut handle) = start(externals, r#"
		external function split(x: stereo) -> (l: mono, r: stereo)

		global module main() -> out: stereo
			l, r = split([1, 2])
			out = l + r
	"#);
	assert_eq!(samples(&mut handle, 1), vec![[12.0, 8.0]]);
}

#[test]
fn function_with_dynamic_arguments_runs_every_sample() {
	let calls = Arc::new(AtomicUsize::new(0));
	let counted = calls.clone();
	let externals = Externals::new().function("double", from_fn(move |x: f64| {
		counted.fetch_add(1, Ordering::SeqCst);
		x * 2.0
	}));
	let (_rt, mut handle) = start(externals, r#"
		external function double(x: mono) -> y: mono

		global module main() -> out: stereo
			t = cell(t + 1, 0)
			out = double(t)
	"#);
	assert_eq!(samples(&mut handle, 3), mono(&[0.0, 2.0, 4.0]));
	assert_eq!(calls.load(Ordering::SeqCst), 3);
}

#[test]
fn function_with_static_arguments_is_evaluated_once() {
	let calls = Arc::new(AtomicUsize::new(0));
	let counted = calls.clone();
	let externals = Externals::new().function("double", from_fn(move |x: f64| {
		counted.fetch_add(1, Ordering::SeqCst);
		x * 2.0
	}));
	let (_rt, mut handle) = start(externals, r#"
		external function double(x: mono) -> y: mono

		global module main() -> out: stereo
			x = double(21)
			out = x
	"#);
	assert_eq!(samples(&mut handle, 3), mono(&[42.0, 42.0, 42.0]));
	assert_eq!(calls.load(Ordering::SeqCst), 1);
}

/// A function with internal, read-only data, shaped like a future `gmdls`.
struct Table {
	data: Vec<f64>,
}

impl ExternalFunction for Table {
	type Inputs = (f64, f64); // row, column
	type Outputs = f64;

	fn call(&mut self, (row, column): (f64, f64)) -> f64 {
		let index = row.round_ties_even() as usize * 2 + column.round_ties_even() as usize;
		self.data.get(index).copied().unwrap_or(0.0)
	}
}

#[test]
fn function_with_internal_data() {
	let externals = Externals::new().function("table", Table { data: vec![1.0, 2.0, 3.0, 4.0] });
	let (_rt, mut handle) = start(externals, r#"
		external function table(row: mono, column: mono) -> out: mono

		global module main() -> out: stereo
			t = cell(t + 1, 0)
			out = table(t, 1)
	"#);
	assert_eq!(samples(&mut handle, 3), mono(&[2.0, 4.0, 0.0]));
}

#[test]
fn bools_cross_as_raw_masks() {
	let seen = Arc::new(Mutex::new(vec![]));
	let recorded = seen.clone();
	let externals = Externals::new().function("invert", from_fn(move |b: f64| {
		recorded.lock().unwrap().push(b.to_bits());
		if b != 0.0 { FALSE } else { TRUE }
	}));
	let (_rt, mut handle) = start(externals, r#"
		external function invert(b: mono bool) -> out: mono bool

		global module main() -> out: stereo
			out = [invert(1 < 2) ? 5 : 7, invert(2 < 1) ? 5 : 7]
	"#);
	assert_eq!(samples(&mut handle, 1), vec![[7.0, 5.0]]);
	let mut seen = seen.lock().unwrap().clone();
	seen.sort();
	assert_eq!(seen, vec![0, u64::MAX]);
}

// ============================================================
// External modules
// ============================================================

#[test]
fn module_has_a_state_per_call_site() {
	let (counter, live_states) = counter();
	let (_rt, mut handle) = start(Externals::new().module("counter", counter), &format!(r#"
		{COUNTER}
		global module main() -> out: stereo
			out = [counter(10, 1), counter(100, 2)]
	"#));
	assert_eq!(samples(&mut handle, 3), vec![[10.0, 100.0], [11.0, 102.0], [12.0, 104.0]]);
	assert_eq!(live_states.load(Ordering::SeqCst), 2);
}

#[test]
fn module_has_a_state_per_note() {
	let (counter, live_states) = counter();
	let (_rt, mut handle) = start(Externals::new().module("counter", counter), &format!(r#"
		{COUNTER}
		instrument beep() -> out: mono
			out = counter(key(), 1)

		global module main() -> out: stereo
			out = 1::beep()
	"#));
	assert_eq!(samples(&mut handle, 1), mono(&[0.0]));
	handle.note_on(0, 60, 100).unwrap();
	assert_eq!(samples(&mut handle, 2), mono(&[60.0, 61.0]));
	handle.note_on(0, 70, 100).unwrap();
	assert_eq!(samples(&mut handle, 2), mono(&[62.0 + 70.0, 63.0 + 71.0]));
	assert_eq!(live_states.load(Ordering::SeqCst), 2);
}

#[test]
fn module_has_a_state_per_repetition() {
	let (counter, live_states) = counter();
	let (_rt, mut handle) = start(Externals::new().module("counter", counter), &format!(r#"
		{COUNTER}
		global module main() -> out: stereo
			out = for i to 3 add counter(i * 10, 1)
	"#));
	assert_eq!(samples(&mut handle, 3), mono(&[30.0, 33.0, 36.0]));
	assert_eq!(live_states.load(Ordering::SeqCst), 3);
}

/// Counts up from its static start value, with no dynamic inputs.
struct Ramp;

impl ExternalModule for Ramp {
	type StaticInputs = (f64,);
	type DynamicInputs = ();
	type Outputs = f64;
	type State = f64;

	fn init(&mut self, (start,): (f64,)) -> f64 {
		start
	}

	fn process(&mut self, value: &mut f64, (): ()) -> f64 {
		*value += 1.0;
		*value - 1.0
	}
}

#[test]
fn module_can_fill_a_buffer() {
	let (_rt, mut handle) = start(Externals::new().module("ramp", Ramp), r#"
		external module ramp(start: static mono) -> out: mono

		global module main() -> out: stereo
			buf: mono buffer = for 4 buffer ramp(10)
			out = [buf[0], buf[3]]
	"#);
	assert_eq!(samples(&mut handle, 2), vec![[10.0, 13.0], [10.0, 13.0]]);
}

// ============================================================
// Checking programs against the registered implementations
// ============================================================

fn submit_error(externals: Externals, src: &str) -> String {
	let (rt, _handle) = default_jingler_runtime(externals).unwrap();
	rt.submit_program(&compile(src)).unwrap_err().to_string()
}

#[test]
fn missing_implementations_are_rejected() {
	let error = submit_error(Externals::new(), &format!(r#"
		{COUNTER}
		external function lookup(id: mono) -> out: mono

		global module main() -> out: stereo
			out = lookup(1) + counter(0, 1)
	"#));
	assert!(error.contains("External function 'lookup' has no implementation."), "{error}");
	assert!(error.contains("External module 'counter' has no implementation."), "{error}");
	assert_eq!(error.lines().count(), 2, "{error}");
}

#[test]
fn mismatched_implementations_are_rejected() {
	let (counter, _) = counter();
	let externals = Externals::new()
		.function("lookup", from_fn(|x: f64| x))
		.module("counter", counter);
	let error = submit_error(externals, r#"
		external function lookup(id: mono, offset: mono) -> out: stereo
		external module counter(start: static stereo, step: mono) -> out: mono

		global module main() -> out: stereo
			out = lookup(1, 2) + counter([0, 0], 1)
	"#);
	assert!(error.contains(
		"External function 'lookup' is declared as (mono, mono) -> (stereo) but implemented as (mono) -> (mono)."),
		"{error}");
	assert!(error.contains(
		"External module 'counter', static part, is declared as (stereo) -> () but implemented as (mono) -> ()."),
		"{error}");
}

#[test]
fn function_and_module_implementations_are_not_interchangeable() {
	let (counter, _) = counter();
	let externals = Externals::new()
		.function("lookup", from_fn(|x: f64| x))
		.module("counter", counter);
	let error = submit_error(externals, r#"
		external module lookup(x: mono) -> out: mono
		external function counter(x: mono) -> out: mono

		global module main() -> out: stereo
			out = lookup(1) + counter(1)
	"#);
	assert!(error.contains("External module 'lookup' is implemented as a function."), "{error}");
	assert!(error.contains("External function 'counter' is implemented as a module."), "{error}");
}

#[test]
fn unused_external_members_need_no_implementation() {
	let (_rt, mut handle) = start(Externals::new(), &format!(r#"
		{COUNTER}
		global module main() -> out: stereo
			out = 1
	"#));
	assert_eq!(samples(&mut handle, 1), mono(&[1.0]));
}

#[test]
fn rejected_program_leaves_the_current_one_playing() {
	let (rt, mut handle) = start(Externals::new(), "global module main() -> out: stereo\n\tout = 1\n");
	let bad = compile(&format!("{COUNTER}global module main() -> out: stereo\n\tout = counter(0, 1)\n"));
	assert!(rt.submit_program(&bad).is_err());
	handle.poll_pending().unwrap();
	assert_eq!(samples(&mut handle, 1), mono(&[1.0]));
}

#[test]
#[should_panic(expected = "External member 'f' registered twice")]
fn registering_a_name_twice_panics() {
	let _ = Externals::new()
		.function("f", from_fn(|x: f64| x))
		.module("f", Ramp);
}

// ============================================================
// Lifetimes of implementations and states
// ============================================================

#[test]
fn states_survive_constant_updates_but_not_reinitialization() {
	let (counter, live_states) = counter();
	let (rt, mut handle) = start(Externals::new().module("counter", counter), &format!(r#"
		{COUNTER}
		global module main() -> out: stereo
			out = counter(10, 1) * 1
	"#));
	assert_eq!(samples(&mut handle, 3), mono(&[10.0, 11.0, 12.0]));

	let updated = compile(&format!("{COUNTER}global module main() -> out: stereo\n\tout = counter(10, 1) * 2\n"));
	assert!(matches!(rt.submit_program(&updated).unwrap(), SubmitOutcome::ConstantUpdate { .. }));
	handle.poll_pending().unwrap();
	assert_eq!(samples(&mut handle, 1), mono(&[26.0]));

	handle.initialize(SAMPLE_RATE).unwrap();
	assert_eq!(samples(&mut handle, 1), mono(&[20.0]));
	assert_eq!(live_states.load(Ordering::SeqCst), 1);
}

#[test]
fn implementations_survive_recompiles_but_states_do_not() {
	let (counter, live_states) = counter();
	let mut calls = 0.0;
	let externals = Externals::new()
		.function("next", from_fn(move |(): ()| {
			calls += 1.0;
			calls
		}))
		.module("counter", counter);
	let (rt, mut handle) = start(externals, &format!(r#"
		{COUNTER}
		external function next() -> n: mono

		global module main() -> out: stereo
			n = next()
			out = [n, counter(0, 1)]
	"#));
	assert_eq!(samples(&mut handle, 2), vec![[1.0, 0.0], [1.0, 1.0]]);
	assert_eq!(live_states.load(Ordering::SeqCst), 1);

	let recompiled = compile(r#"
		external function next() -> n: mono

		global module main() -> out: stereo
			n = next() + 0
			out = n
	"#);
	assert_eq!(rt.submit_program(&recompiled).unwrap(), SubmitOutcome::FreshCompile);
	handle.poll_pending().unwrap();
	assert_eq!(samples(&mut handle, 2), mono(&[2.0, 2.0]));
	assert_eq!(live_states.load(Ordering::SeqCst), 0);
}

// ============================================================
// Panics
// ============================================================

fn checked_double() -> impl ExternalFunction {
	from_fn(|x: f64| {
		assert!(x >= 0.0, "negative input");
		x * 2.0
	})
}

#[test]
fn panic_in_implementation_is_an_error() {
	let (_rt, mut handle) = start(Externals::new().function("double", checked_double()), r#"
		external function double(x: mono) -> y: mono

		global module main() -> out: stereo
			t = cell(t - 1, 1)
			out = double(t)
	"#);
	assert_eq!(samples(&mut handle, 2), mono(&[2.0, 0.0]));
	let error = format!("{:?}", handle.next_sample().unwrap_err());
	assert!(error.contains("External member 'double' panicked: negative input"), "{error}");
}

#[test]
fn panic_while_installing_keeps_the_implementations() {
	let good = compile(r#"
		external function double(x: mono) -> y: mono

		global module main() -> out: stereo
			t = cell(t + 1, 0)
			out = double(t)
	"#);
	let bad = compile(r#"
		external function double(x: mono) -> y: mono

		global module main() -> out: stereo
			x = double(-1)
			out = x
	"#);

	// No program installed yet
	let (rt, mut handle) = default_jingler_runtime(Externals::new().function("double", checked_double())).unwrap();
	handle.initialize(SAMPLE_RATE).unwrap();
	rt.submit_program(&bad).unwrap();
	assert!(handle.poll_pending().is_err());
	rt.submit_program(&good).unwrap();
	handle.poll_pending().unwrap();
	assert_eq!(samples(&mut handle, 2), mono(&[0.0, 2.0]));

	// A program still playing
	rt.submit_program(&bad).unwrap();
	assert!(handle.poll_pending().is_err());
	assert_eq!(samples(&mut handle, 1), mono(&[4.0]));
}

// ============================================================
// Compiling to Wasm without implementations
// ============================================================

#[test]
fn compiled_module_imports_its_external_members() {
	let program = compile(r#"
		external function lookup(id: mono, offset: mono) -> out: stereo
		external module osc(freq: static mono, shape: mono) -> out: stereo
		external function unused(x: mono) -> y: mono

		global module main() -> out: stereo
			out = lookup(1, 2) + osc(440, 0.5)
	"#);
	let wasm = compile_wasm(&program).unwrap();
	let module = wasmtime::Module::new(&wasmtime::Engine::default(), &wasm).unwrap();
	let describe = |types: &mut dyn Iterator<Item = wasmtime::ValType>| {
		types.map(|t| t.to_string()).collect::<Vec<_>>().join(" ")
	};
	let mut imports: Vec<String> = module.imports()
		.filter(|import| import.module() == "external")
		.map(|import| {
			let ty = import.ty().unwrap_func().clone();
			format!("{}: {} -> {}", import.name(), describe(&mut ty.params()), describe(&mut ty.results()))
		})
		.collect();
	imports.sort();
	assert_eq!(imports, vec![
		"lookup: f64 f64 -> f64 f64",
		"osc.dynamic: i32 f64 -> f64 f64",
		"osc.static: f64 -> i32",
	]);
}

#[test]
fn invalid_external_procedures_are_rejected() {
	let program = compile(r#"
		external function f(x: mono) -> y: mono

		global module main() -> out: stereo
			t = cell(t + 1, 0)
			out = f(t)
	"#);

	let mut out_of_range = program.clone();
	for procedure in &mut out_of_range.procedures {
		for instr in &mut procedure.code {
			if let ir::Instruction::CallExternal(..) = instr {
				*instr = ir::Instruction::CallExternal(1);
			}
		}
	}
	let error = compile_wasm(&out_of_range).unwrap_err().to_string();
	assert!(error.contains("Invalid CallExternal(1) in procedure 'main'"), "{error}");

	let mut instrument = program.clone();
	instrument.externals[0].kind = ir::ProcedureKind::Instrument { scope: ir::Scope::Dynamic };
	let error = compile_wasm(&instrument).unwrap_err().to_string();
	assert!(error.contains("Unsupported external procedure: f [external instrument, dynamic part]"), "{error}");
}
