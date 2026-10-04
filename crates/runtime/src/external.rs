//! External members: functions and modules declared in Zing with `external`
//! and no body, whose implementations are supplied by the embedder.

use std::any::Any;
use std::marker::PhantomData;
use std::panic::{AssertUnwindSafe, catch_unwind};

use anyhow::{Result, anyhow};
use wasmtime::Val;

/// A Zing `true` as seen by an implementation: a mask with all bits set.
pub const TRUE: f64 = f64::from_bits(u64::MAX);
/// A Zing `false` as seen by an implementation.
pub const FALSE: f64 = 0.0;

mod sealed {
	pub trait Sealed {}
}

/// A single Zing value: `f64` for mono, `[f64; 2]` (left, right) for stereo.
///
/// Bools are passed as raw masks: an input is either `TRUE` or `FALSE`
/// (test it with `x != 0.0`), and a bool output must be exactly one of them.
pub trait Value: sealed::Sealed + Copy + 'static {
	#[doc(hidden)]
	const WIDTH: ir::Width;
	#[doc(hidden)]
	fn read(lanes: &mut impl Iterator<Item = f64>) -> Self;
	#[doc(hidden)]
	fn write(self, lanes: &mut impl FnMut(f64));
}

/// The inputs or outputs of an external member: `()`, a single [`Value`], or
/// a tuple of values, in declaration order.
pub trait Values: sealed::Sealed + Sized + 'static {
	#[doc(hidden)]
	fn widths() -> Vec<ir::Width>;
	#[doc(hidden)]
	fn read(lanes: &mut impl Iterator<Item = f64>) -> Self;
	#[doc(hidden)]
	fn write(self, lanes: &mut impl FnMut(f64));
}

impl sealed::Sealed for f64 {}

impl Value for f64 {
	const WIDTH: ir::Width = ir::Width::Mono;

	fn read(lanes: &mut impl Iterator<Item = f64>) -> Self {
		lanes.next().expect("missing mono value")
	}

	fn write(self, lanes: &mut impl FnMut(f64)) {
		lanes(self);
	}
}

impl sealed::Sealed for [f64; 2] {}

impl Value for [f64; 2] {
	const WIDTH: ir::Width = ir::Width::Stereo;

	fn read(lanes: &mut impl Iterator<Item = f64>) -> Self {
		let left = lanes.next().expect("missing left value");
		let right = lanes.next().expect("missing right value");
		[left, right]
	}

	fn write(self, lanes: &mut impl FnMut(f64)) {
		lanes(self[0]);
		lanes(self[1]);
	}
}

macro_rules! single_values {
	($($t:ty),*) => {$(
		impl Values for $t {
			fn widths() -> Vec<ir::Width> {
				vec![<$t as Value>::WIDTH]
			}

			fn read(lanes: &mut impl Iterator<Item = f64>) -> Self {
				<$t as Value>::read(lanes)
			}

			fn write(self, lanes: &mut impl FnMut(f64)) {
				<$t as Value>::write(self, lanes)
			}
		}
	)*};
}

single_values!(f64, [f64; 2]);

macro_rules! tuple_values {
	($($t:ident $v:ident),*) => {
		impl<$($t: Value),*> sealed::Sealed for ($($t,)*) {}

		#[allow(unused_variables)]
		impl<$($t: Value),*> Values for ($($t,)*) {
			fn widths() -> Vec<ir::Width> {
				vec![$($t::WIDTH),*]
			}

			fn read(lanes: &mut impl Iterator<Item = f64>) -> Self {
				($($t::read(lanes),)*)
			}

			fn write(self, lanes: &mut impl FnMut(f64)) {
				let ($($v,)*) = self;
				$($v.write(lanes);)*
			}
		}
	};
}

tuple_values!();
tuple_values!(A a);
tuple_values!(A a, B b);
tuple_values!(A a, B b, C c);
tuple_values!(A a, B b, C c, D d);
tuple_values!(A a, B b, C c, D d, E e);
tuple_values!(A a, B b, C c, D d, E e, F f);
tuple_values!(A a, B b, C c, D d, E e, F f, G g);
tuple_values!(A a, B b, C c, D d, E e, F f, G g, H h);
tuple_values!(A a, B b, C c, D d, E e, F f, G g, H h, I i);
tuple_values!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j);
tuple_values!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k);
tuple_values!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k, L l);

/// Implementation of an external function.
///
/// External functions are pure by contract: the same inputs must give the
/// same outputs, so a call may be evaluated once in the static phase. The
/// implementation may keep read-only data or private caches.
pub trait ExternalFunction: Send + 'static {
	type Inputs: Values;
	type Outputs: Values;

	fn call(&mut self, inputs: Self::Inputs) -> Self::Outputs;
}

/// Implementation of an external module.
///
/// Every call site (per note in note context, and per `for` iteration) gets
/// its own state: `init` creates it from the static inputs when the static
/// part runs, and `process` advances it with the dynamic inputs every sample.
/// States are dropped when the program is re-initialized or replaced.
pub trait ExternalModule: Send + 'static {
	type StaticInputs: Values;
	type DynamicInputs: Values;
	type Outputs: Values;
	type State: Send + 'static;

	fn init(&mut self, inputs: Self::StaticInputs) -> Self::State;
	fn process(&mut self, state: &mut Self::State, inputs: Self::DynamicInputs) -> Self::Outputs;
}

/// An external function implemented by a closure taking all inputs as one
/// argument, e.g. `from_fn(|(id, offset): (f64, f64)| -> [f64; 2] { … })`.
pub fn from_fn<I, O, F>(f: F) -> FnFunction<I, O, F>
where
	I: Values,
	O: Values,
	F: FnMut(I) -> O + Send + 'static,
{
	FnFunction { f, signature: PhantomData }
}

/// See [`from_fn`].
pub struct FnFunction<I, O, F> {
	f: F,
	signature: PhantomData<fn(I) -> O>,
}

impl<I, O, F> ExternalFunction for FnFunction<I, O, F>
where
	I: Values,
	O: Values,
	F: FnMut(I) -> O + Send + 'static,
{
	type Inputs = I;
	type Outputs = O;

	fn call(&mut self, inputs: I) -> O {
		(self.f)(inputs)
	}
}

/// Implementations of external members, registered by name when the
/// runtime is created.
#[derive(Default)]
pub struct Externals {
	signatures: ExternalSignatures,
	implementations: Implementations,
}

impl Externals {
	pub fn new() -> Self {
		Self::default()
	}

	/// Register the implementation of `external function <name>`.
	pub fn function(mut self, name: impl Into<String>, implementation: impl ExternalFunction) -> Self {
		let name = self.new_name(name.into());
		self.signatures.functions.push((name, function_signature(&implementation)));
		self.implementations.functions.push(Box::new(implementation));
		self
	}

	/// Register the implementation of `external module <name>`.
	pub fn module(mut self, name: impl Into<String>, implementation: impl ExternalModule) -> Self {
		let name = self.new_name(name.into());
		self.signatures.modules.push((name, module_signature(&implementation)));
		self.implementations.modules.push(Box::new(implementation));
		self
	}

	fn new_name(&self, name: String) -> String {
		let functions = self.signatures.functions.iter().map(|(n, _)| n);
		let modules = self.signatures.modules.iter().map(|(n, _)| n);
		let registered = functions.chain(modules).any(|existing| *existing == name);
		assert!(!registered, "External member '{}' registered twice", name);
		name
	}

	pub(crate) fn into_parts(self) -> (ExternalSignatures, Implementations) {
		(self.signatures, self.implementations)
	}
}

fn function_signature<F: ExternalFunction>(_: &F) -> Signature {
	Signature {
		inputs: F::Inputs::widths(),
		outputs: F::Outputs::widths(),
	}
}

fn module_signature<M: ExternalModule>(_: &M) -> ModuleSignature {
	ModuleSignature {
		static_inputs: M::StaticInputs::widths(),
		dynamic: Signature {
			inputs: M::DynamicInputs::widths(),
			outputs: M::Outputs::widths(),
		},
	}
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Signature {
	pub inputs: Vec<ir::Width>,
	pub outputs: Vec<ir::Width>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ModuleSignature {
	pub static_inputs: Vec<ir::Width>,
	pub dynamic: Signature,
}

/// The signatures of the registered implementations, in registration order.
#[derive(Default)]
pub(crate) struct ExternalSignatures {
	pub functions: Vec<(String, Signature)>,
	pub modules: Vec<(String, ModuleSignature)>,
}

impl ExternalSignatures {
	/// Check that every external procedure of `program` has a registered
	/// implementation with a matching signature.
	pub fn check_program(&self, program: &ir::Program) -> Result<()> {
		use ir::ProcedureKind::*;
		let mut problems: Vec<String> = vec![];
		let mut report = |problem: String| {
			// A module has both a static and a dynamic part; report it once.
			if !problems.contains(&problem) {
				problems.push(problem);
			}
		};
		for external in &program.externals {
			let name = &external.name;
			let function = self.functions.iter().find(|(n, _)| n == name).map(|(_, s)| s);
			let module = self.modules.iter().find(|(n, _)| n == name).map(|(_, s)| s);
			let (declared, implemented) = match (external.kind, function, module) {
				(Instrument { .. }, ..) => {
					report(format!("External instrument '{}' is not supported.", name));
					continue;
				},
				(Function, Some(signature), _) => (
					describe(&external.inputs, &external.outputs),
					describe_widths(&signature.inputs, &signature.outputs),
				),
				(Module { scope: ir::Scope::Static }, _, Some(signature)) => (
					describe(&external.inputs, &external.outputs),
					describe_widths(&signature.static_inputs, &[]),
				),
				(Module { scope: ir::Scope::Dynamic }, _, Some(signature)) => (
					describe(&external.inputs, &external.outputs),
					describe_widths(&signature.dynamic.inputs, &signature.dynamic.outputs),
				),
				(Function, None, Some(_)) => {
					report(format!("External function '{}' is implemented as a module.", name));
					continue;
				},
				(Module { .. }, Some(_), None) => {
					report(format!("External module '{}' is implemented as a function.", name));
					continue;
				},
				(Function, None, None) => {
					report(format!("External function '{}' has no implementation.", name));
					continue;
				},
				(Module { .. }, None, None) => {
					report(format!("External module '{}' has no implementation.", name));
					continue;
				},
			};
			if declared != implemented {
				let part = match external.kind {
					Module { scope } => format!("module '{}', {} part,", name, scope),
					_ => format!("function '{}'", name),
				};
				report(format!("External {} is declared as {} but implemented as {}.",
					part, declared, implemented));
			}
		}
		if problems.is_empty() {
			Ok(())
		} else {
			Err(anyhow!("{}", problems.join("\n")))
		}
	}
}

fn describe(inputs: &[ir::Type], outputs: &[ir::Type]) -> String {
	let describe_list = |types: &[ir::Type]| -> Vec<String> {
		types.iter().map(|t| match t.value_type {
			ir::ValueType::Number => t.width.to_string(),
			ir::ValueType::Buffer => format!("{} buffer", t.width),
		}).collect()
	};
	format!("({}) -> ({})", describe_list(inputs).join(", "), describe_list(outputs).join(", "))
}

fn describe_widths(inputs: &[ir::Width], outputs: &[ir::Width]) -> String {
	let as_types = |widths: &[ir::Width]| -> Vec<ir::Type> {
		widths.iter().map(|&width| ir::Type { width, value_type: ir::ValueType::Number }).collect()
	};
	describe(&as_types(inputs), &as_types(outputs))
}

/// Number of `f64` values that carry a list of values across the Wasm boundary.
pub(crate) fn lane_count(widths: impl IntoIterator<Item = ir::Width>) -> usize {
	widths.into_iter().map(|w| match w {
		ir::Width::Mono => 1,
		ir::Width::Stereo => 2,
		ir::Width::Generic => panic!("Generic width in external member"),
	}).sum()
}

trait ErasedFunction: Send {
	fn call_erased(&mut self, params: &[Val], results: &mut [Val]);
}

impl<F: ExternalFunction> ErasedFunction for F {
	fn call_erased(&mut self, params: &[Val], results: &mut [Val]) {
		let inputs = F::Inputs::read(&mut params.iter().map(Val::unwrap_f64));
		write_results(self.call(inputs), results);
	}
}

trait ErasedModule: Send {
	fn new_states(&self) -> Box<dyn Any + Send>;
	fn init_erased(&mut self, states: &mut (dyn Any + Send), params: &[Val]) -> i32;
	fn process_erased(&mut self, states: &mut (dyn Any + Send), params: &[Val], results: &mut [Val]) -> Result<()>;
}

impl<M: ExternalModule> ErasedModule for M {
	fn new_states(&self) -> Box<dyn Any + Send> {
		Box::new(Vec::<M::State>::new())
	}

	fn init_erased(&mut self, states: &mut (dyn Any + Send), params: &[Val]) -> i32 {
		let states = states.downcast_mut::<Vec<M::State>>().expect("state type");
		let inputs = M::StaticInputs::read(&mut params.iter().map(Val::unwrap_f64));
		states.push(self.init(inputs));
		(states.len() - 1) as i32
	}

	fn process_erased(&mut self, states: &mut (dyn Any + Send), params: &[Val], results: &mut [Val]) -> Result<()> {
		let states = states.downcast_mut::<Vec<M::State>>().expect("state type");
		let handle = params[0].unwrap_i32();
		let state = states.get_mut(handle as usize)
			.ok_or_else(|| anyhow!("Invalid state handle {}", handle))?;
		let inputs = M::DynamicInputs::read(&mut params[1..].iter().map(Val::unwrap_f64));
		write_results(self.process(state, inputs), results);
		Ok(())
	}
}

fn write_results(outputs: impl Values, results: &mut [Val]) {
	let mut results = results.iter_mut();
	outputs.write(&mut |lane| *results.next().expect("missing result") = Val::F64(lane.to_bits()));
}

/// The registered implementations, indexed like `ExternalSignatures`.
#[derive(Default)]
pub(crate) struct Implementations {
	functions: Vec<Box<dyn ErasedFunction>>,
	modules: Vec<Box<dyn ErasedModule>>,
}

/// The external-member data of one Wasm store: the implementations, which
/// move from program to program, and the module states of this program.
pub(crate) struct ExternalData {
	pub implementations: Option<Implementations>,
	/// A `Vec<M::State>` per registered module, created on first use.
	states: Vec<Option<Box<dyn Any + Send>>>,
}

impl ExternalData {
	pub fn new(module_count: usize) -> Self {
		ExternalData {
			implementations: None,
			states: (0..module_count).map(|_| None).collect(),
		}
	}

	pub fn clear_states(&mut self) {
		for states in &mut self.states {
			*states = None;
		}
	}

	pub fn call_function(&mut self, index: usize, name: &str, params: &[Val], results: &mut [Val]) -> Result<()> {
		let function = &mut installed(&mut self.implementations)?.functions[index];
		catch_panic(name, || function.call_erased(params, results))
	}

	pub fn init_module(&mut self, index: usize, name: &str, params: &[Val]) -> Result<i32> {
		let module = &mut installed(&mut self.implementations)?.modules[index];
		let states = self.states[index].get_or_insert_with(|| module.new_states());
		catch_panic(name, || module.init_erased(states.as_mut(), params))
	}

	pub fn process_module(&mut self, index: usize, name: &str, params: &[Val], results: &mut [Val]) -> Result<()> {
		let module = &mut installed(&mut self.implementations)?.modules[index];
		let states = self.states[index].as_mut()
			.ok_or_else(|| anyhow!("External module '{}' processed before its static part ran", name))?;
		catch_panic(name, || module.process_erased(states.as_mut(), params, results))?
	}
}

fn installed(implementations: &mut Option<Implementations>) -> Result<&mut Implementations> {
	implementations.as_mut().ok_or_else(|| anyhow!("External member implementations are not installed"))
}

/// Turn a panic in an implementation into an error, which traps the Wasm code.
fn catch_panic<R>(name: &str, f: impl FnOnce() -> R) -> Result<R> {
	catch_unwind(AssertUnwindSafe(f)).map_err(|payload| {
		let message = payload.downcast_ref::<&str>().copied()
			.or_else(|| payload.downcast_ref::<String>().map(String::as_str))
			.unwrap_or("unknown panic");
		anyhow!("External member '{}' panicked: {}", name, message)
	})
}
