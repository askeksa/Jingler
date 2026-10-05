# External members

An **external member** is a Zing function or module declared with `external` and no body. The **embedder** supplies its **implementation**. The embedder is the program that runs the compiled Zing program: the VST plugin, `zing-cmd`, a test, or your own code. External members let a patch use things that can't be written in Zing, such as sample data, lookup tables or native DSP code.

This document covers the three layers involved:

- [Zing](#zing): declaring and calling external members.
- [Runtime API](#runtime-api): implementing them in Rust for the Jingler runtime.
- [Wasm API](#wasm-api): supplying them yourself when you instantiate a compiled Wasm module.

The same example runs through all three layers.

## Concepts

A module call has two halves, and an external module has the same two halves:

- The **static part** runs once when a **state** is created. That happens at startup for a call in global context, and at note-on for a call in note context. It sees only the static inputs.
- The **dynamic part** runs every sample. It advances the state using the dynamic inputs and produces the outputs.

A **state** is the data that one call of a module keeps between samples. There is one state per call site, one per note when called in note context, and one per iteration of a `for i to N` loop. For a Zing module, the state is its cells and delay lines. For an external module, it is whatever value the implementation creates.

An **external function** has no state and no phases of its own. It must be **pure**: the same inputs always give the same outputs.

## Zing

```zing
external function pan(x: mono, position: mono) -> out: stereo
external module pulse(freq: static mono, width: mono, rate: static mono) -> out: mono

global module main() -> out: stereo
	out = pan(pulse(440, 0.25, samplerate()), 0.3)
```

You call an external member like any other member. The declaration is a normal member header with `external` in front and no body. Declarations can sit in included files.

### Rules

- Every input and output needs an explicit type: `mono` or `stereo`, optionally followed by `bool`, e.g. `stereo bool`. You can't use `generic` or `buffer`.
- Only functions and modules can be external. The following are rejected:
  - instruments;
  - `global` or `note` prefixes;
  - MIDI inputs;
  - a body;
  - an external `main`.
- All other member rules still apply:
  - Function inputs and outputs can't be marked `static` or `dynamic`.
  - Module inputs are dynamic unless marked `static`.
  - Module outputs can't be static.
  - Modules can't be called from functions.
  - External members share the member namespace and can't shadow builtins.
- Members that `main` never reaches are dropped from the program. They don't need an implementation.

### When external functions run

An external function call runs in the phase of the statement that contains it, just like a Zing function:

```zing
	x = pan(1, 0.5)       # static arguments, static variable: runs once, at startup / note-on
	out = pan(1, 0.5)     # same call, assigned to a dynamic output: runs every sample
```

Because external functions are pure by contract, the compiler may evaluate a call statically. It may also call the function more or fewer times than the source suggests. So an implementation must not have observable side effects, and must not depend on anything that changes over time, such as host tempo or a clock. Read-only data and private caches are fine. If you need time-varying input or memory per call site, use an external module.

### External modules and states

Each call of an external module gets its own state:

- per call site;
- per note, when called in note context;
- per iteration of a `for i to N` loop, e.g. `for i to 3 add pulse(i * 110, 0.5, samplerate())`.

A module whose inputs are all static can fill a buffer. This creates one state, and the dynamic part is called once per element:

```zing
external module ramp(start: static mono) -> out: mono
	...
	buf: mono buffer = for 64 buffer ramp(10)   # ramp's dynamic part fills buf[0] .. buf[63]
```

As with cells, a static input is read only when a state is created. Changing a constant that feeds a static input therefore affects only states created after the change.

## Runtime API

Pass the implementations to the runtime when you create it. They can't be changed afterwards.

```rust
use runtime::{ExternalModule, Externals, default_jingler_runtime, from_fn};

struct Pulse;

struct PulseState {
	phase: f64,
	step: f64,
}

impl ExternalModule for Pulse {
	type StaticInputs = (f64, f64); // freq, rate
	type DynamicInputs = f64;       // width
	type Outputs = f64;             // out
	type State = PulseState;

	fn init(&mut self, (freq, rate): (f64, f64)) -> PulseState {
		PulseState { phase: 0.0, step: freq / rate }
	}

	fn process(&mut self, state: &mut PulseState, width: f64) -> f64 {
		let out = if state.phase < width { 1.0 } else { -1.0 };
		state.phase = (state.phase + state.step).fract();
		out
	}
}

let externals = Externals::new()
	.function("pan", from_fn(|(x, position): (f64, f64)| [x * (1.0 - position), x * position]))
	.module("pulse", Pulse);
let (runtime, handle) = default_jingler_runtime(externals)?;
```

Use `Externals::new()` if you implement no external members. Registering the same name twice panics.

### Traits

```rust
pub trait ExternalFunction: Send + 'static {
	type Inputs: Values;
	type Outputs: Values;
	fn call(&mut self, inputs: Self::Inputs) -> Self::Outputs;
}

pub trait ExternalModule: Send + 'static {
	type StaticInputs: Values;
	type DynamicInputs: Values;
	type Outputs: Values;
	type State: Send + 'static;
	fn init(&mut self, inputs: Self::StaticInputs) -> Self::State;
	fn process(&mut self, state: &mut Self::State, inputs: Self::DynamicInputs) -> Self::Outputs;
}
```

`from_fn(closure)` turns a closure that takes all inputs as one argument into an `ExternalFunction`. For an external function with more context, implement the trait yourself. For example, an implementation can own a table or file data as a field and read it through `&mut self`.

### Values

| Zing | Rust |
|---|---|
| `mono` | `f64` |
| `stereo` | `[f64; 2]` (left, right) |
| `mono bool` / `stereo bool` | the same as above, holding `TRUE` or `FALSE` (see below) |
| no values | `()` |
| one value | the value itself, e.g. `f64` |
| several values | a tuple, e.g. `(f64, [f64; 2])`, up to 12 |

Values keep their declaration order. A module's inputs are split into two lists, `StaticInputs` and `DynamicInputs`, and each keeps the declaration order of its own inputs. In `pulse(freq: static mono, width: mono, rate: static mono)`, the static inputs are `(freq, rate)` and the dynamic input is `width`.

**Bools** are raw bit masks, not `0.0`/`1.0`:

- A bool input is either `runtime::TRUE` (all bits set, which is a NaN) or `runtime::FALSE` (`0.0`). Test it with `x != 0.0`.
- A bool output must be exactly `TRUE` or `FALSE`. Any other value silently breaks conditionals.
- The runtime can't tell whether a value is a bool or a number. Declaring `bool` in Zing and treating the value as a number in Rust, or the reverse, is not detected.

### Checking programs against implementations

`submit_program` checks every external member that the program uses against the registered implementations. If anything doesn't match, it returns `Err` and the current program keeps playing. The error has one line per problem, for example:

```
External function 'pan' has no implementation.
External module 'pulse' is implemented as a function.
External module 'pulse', static part, is declared as (mono, mono) -> () but implemented as (mono) -> ().
```

The check compares names, kinds (function or module) and the widths of each input and output list. A module's static part and dynamic part are checked separately.

### Lifetimes and threading

- **Implementations** live for as long as the runtime. When a structurally changed program is installed, they move to the new program. This means data held in an implementation survives recompiles.
- **States** belong to one program:
  - They are created by `init`.
  - They are kept across constants-only updates.
  - They are dropped when the program is re-initialized (`initialize`, e.g. after a sample-rate change) or replaced by a structurally different program.
  - They are **not** dropped when a note ends. The states of finished notes stay in memory until the next re-initialize.
- **Threading**: all calls (`call`, `init`, `process`) happen on the audio thread, inside the runtime handle's methods. Avoid blocking work there. Implementations get no context such as sample rate or tempo. Pass what they need as inputs, e.g. `samplerate()` as a static input, as `pulse` does above.
- **Panics**: a panic in an implementation is caught. The handle method that was running returns `Err` with the message `External member '<name>' panicked: …`.

### Compiling without implementations

`runtime::compile_wasm(&program)` produces the Wasm module without instantiating it, so it needs no implementations. `zing-cmd --write-wasm` uses it. The external members remain imports for whoever instantiates the module. See the next section.

## Wasm API

A compiled program imports its external members from the Wasm import module `external`. It only imports the members it uses. Values travel as plain `f64` lanes: one lane for `mono`, two for `stereo` (left, then right). Bools use the same masks as in Rust.

| Zing | Import name | Parameters | Results |
|---|---|---|---|
| `external function f` | `f` | input lanes | output lanes |
| `external module m`, static part | `m.static` | static input lanes | `i32` state handle |
| `external module m`, dynamic part | `m.dynamic` | `i32` state handle, dynamic input lanes | output lanes |

For the example program:

```wat
(import "external" "pan"           (func (param f64 f64) (result f64 f64)))
(import "external" "pulse.static"  (func (param f64 f64) (result i32)))   ;; freq, rate
(import "external" "pulse.dynamic" (func (param i32 f64) (result f64)))   ;; handle, width
```

The **state handle** is an opaque `i32` that you choose. `m.static` is called whenever a state is created: at `initialize` for calls in global context, and at `note_on` for calls in note context. It runs once per call site, per note and per `for i to N` iteration. A `for N buffer` fill calls `m.static` once and then `m.dynamic` once per element, all during `initialize` or `note_on`. The compiled code stores the handle it returns and passes it back to every `m.dynamic` call for that state. Calling `initialize` again creates all global states again, so you can discard the old ones at that point. A trap in an import aborts the exported function that was running.

The function-purity and bool-mask rules from the previous sections apply here too.

Besides `external`, a compiled module imports `math.*` (`atan2`, `cos`, `exp2`, `log2`, `pow`, `sin`, `sincos`, `tan`) and `gmdls.sample`.

## Limitations

- The NASM player doesn't support external members. `zing-cmd --write-source` rejects programs that use them.
- No current tool registers any implementations:
  - The VST plugin rejects programs that use external members.
  - `zing-cmd --play` and `--write-wav` reject them too.
  - `--write-wasm` and `--print-ir` do work with them. `--connect` sends them, but the plugin then rejects them.
- External members can't have `generic` or `buffer` inputs or outputs.
- Module states of finished notes are not dropped until the program is re-initialized or replaced.
