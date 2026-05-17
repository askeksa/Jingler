mod wasm;

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::Result;

use ir::diff::{Diff, diff_programs};

use crate::wasm::{WasmCompiler, WasmInstanceInner};

/// Listener-side handle to the Jingler runtime. Cheap to clone via `Arc`.
///
/// Programs are submitted here; the runtime compares each new program against
/// the last accepted one and either compiles a fresh Wasm module (structural
/// change) or queues constant-value updates (only `Constant(u32)` values
/// differed). All staged work is picked up on the audio side by
/// `JinglerRuntimeHandle::poll_pending`.
pub trait JinglerRuntime: Send + Sync {
	fn submit_program(&self, program: &ir::Program) -> Result<SubmitOutcome>;
}

/// Audio-side handle. Owned by the audio thread (or the test driver).
pub trait JinglerRuntimeHandle: Send {
	/// Drain anything staged by the listener side: install a freshly compiled
	/// instance if one is pending, then apply any queued constant updates to
	/// the currently installed instance. Cheap when nothing is pending (one
	/// relaxed atomic load).
	fn poll_pending(&mut self) -> Result<()>;

	/// Cache the sample rate. If an instance is already installed, also
	/// re-initialize it. Subsequent structural swaps via `poll_pending` will
	/// auto-initialize with the cached rate.
	fn initialize(&mut self, sample_rate: f32) -> Result<()>;

	fn next_sample(&mut self) -> Result<[f64; 2]>;
	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()>;
	fn note_off(&mut self, channel: u8, note: u8) -> Result<()>;
	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()>;

	/// `None` until at least one program has been installed.
	fn dump(&self) -> Option<&[u8]>;

	/// Read back the currently-installed constant slot values. Returns an
	/// empty vector when no instance is installed. Used by tests to verify
	/// the constants-only update path; also useful for debugging.
	fn current_constants(&mut self) -> Vec<f32>;
}

/// Classification of a `submit_program` call.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubmitOutcome {
	/// First submission, or a structural change → a new Wasm module has been
	/// compiled and staged for the audio thread to install.
	FreshCompile,
	/// Only `Constant(u32)` values differed; `count` updates queued.
	ConstantUpdate { count: usize },
	/// Submitted program is byte-identical (post-diff) to the last accepted
	/// one — nothing was staged.
	NoChange,
}

struct Inner {
	compiler: WasmCompiler,
	pending_flag: AtomicBool,
	pending: Mutex<Pending>,
	listener_state: Mutex<ListenerState>,
}

struct Pending {
	new_instance: Option<WasmInstanceInner>,
	constant_updates: HashMap<u32, f32>,
}

struct ListenerState {
	last_accepted: Option<ir::Program>,
}

struct JinglerRuntimeImpl {
	inner: Arc<Inner>,
}

impl JinglerRuntime for JinglerRuntimeImpl {
	fn submit_program(&self, program: &ir::Program) -> Result<SubmitOutcome> {
		// Hold the listener-state lock for the duration of comparison +
		// compile + stage. This serialises listener submissions but does not
		// block the audio thread (which only locks `pending`).
		let mut listener = self.inner.listener_state.lock().unwrap();

		let diff = match listener.last_accepted.as_ref() {
			Some(old) => diff_programs(old, program),
			None => Diff::Structural,
		};

		match diff {
			Diff::Identical => Ok(SubmitOutcome::NoChange),
			Diff::ConstantsOnly(changes) => {
				let count = changes.len();
				{
					let mut pending = self.inner.pending.lock().unwrap();
					for (slot, bits) in changes {
						pending.constant_updates.insert(slot, f32::from_bits(bits));
					}
				}
				self.inner.pending_flag.store(true, Ordering::Release);
				listener.last_accepted = Some(program.clone());
				Ok(SubmitOutcome::ConstantUpdate { count })
			}
			Diff::Structural => {
				// Compile outside the pending lock — this is the expensive step.
				let instance = self.inner.compiler.compile_and_instantiate(program)?;
				{
					let mut pending = self.inner.pending.lock().unwrap();
					pending.new_instance = Some(instance);
					// Old slot indices may not map onto the new program; the
					// new instance's globals are pre-baked with new values.
					pending.constant_updates.clear();
				}
				self.inner.pending_flag.store(true, Ordering::Release);
				listener.last_accepted = Some(program.clone());
				Ok(SubmitOutcome::FreshCompile)
			}
		}
	}
}

struct JinglerRuntimeHandleImpl {
	inner: Arc<Inner>,
	instance: Option<WasmInstanceInner>,
	sample_rate: Option<f32>,
}

impl JinglerRuntimeHandle for JinglerRuntimeHandleImpl {
	fn poll_pending(&mut self) -> Result<()> {
		if !self.inner.pending_flag.load(Ordering::Acquire) {
			return Ok(());
		}
		// If the listener happens to hold the lock right now, leave the flag
		// set and try again next buffer.
		let Ok(mut pending) = self.inner.pending.try_lock() else {
			return Ok(());
		};
		// Clear the flag while holding the lock — any concurrent submit will
		// re-set it after dropping its lock.
		self.inner.pending_flag.store(false, Ordering::Release);

		let new_instance = pending.new_instance.take();
		let updates: Vec<(u32, f32)> = pending.constant_updates.drain().collect();
		drop(pending);

		if let Some(mut inst) = new_instance {
			if let Some(sr) = self.sample_rate {
				inst.initialize(sr)?;
			}
			self.instance = Some(inst);
		}

		if let Some(inst) = self.instance.as_mut() {
			for (slot, value) in updates {
				inst.set_constant(slot, value)?;
			}
		}

		Ok(())
	}

	fn initialize(&mut self, sample_rate: f32) -> Result<()> {
		self.sample_rate = Some(sample_rate);
		if let Some(inst) = self.instance.as_mut() {
			inst.initialize(sample_rate)?;
		}
		Ok(())
	}

	fn next_sample(&mut self) -> Result<[f64; 2]> {
		match self.instance.as_mut() {
			Some(inst) => inst.next_sample(),
			None => Ok([0.0, 0.0]),
		}
	}

	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()> {
		if let Some(inst) = self.instance.as_mut() {
			inst.note_on(channel, note, velocity)?;
		}
		Ok(())
	}

	fn note_off(&mut self, channel: u8, note: u8) -> Result<()> {
		if let Some(inst) = self.instance.as_mut() {
			inst.note_off(channel, note)?;
		}
		Ok(())
	}

	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()> {
		if let Some(inst) = self.instance.as_mut() {
			inst.set_parameter(index, value)?;
		}
		Ok(())
	}

	fn dump(&self) -> Option<&[u8]> {
		self.instance.as_ref().map(|i| i.dump())
	}

	fn current_constants(&mut self) -> Vec<f32> {
		match self.instance.as_mut() {
			Some(inst) => inst.current_constants(),
			None => Vec::new(),
		}
	}
}

pub fn default_jingler_runtime() -> Result<(Arc<dyn JinglerRuntime>, Box<dyn JinglerRuntimeHandle>)> {
	let compiler = WasmCompiler::new()?;
	let inner = Arc::new(Inner {
		compiler,
		pending_flag: AtomicBool::new(false),
		pending: Mutex::new(Pending {
			new_instance: None,
			constant_updates: HashMap::new(),
		}),
		listener_state: Mutex::new(ListenerState {
			last_accepted: None,
		}),
	});
	let rt: Arc<dyn JinglerRuntime> = Arc::new(JinglerRuntimeImpl { inner: inner.clone() });
	let handle: Box<dyn JinglerRuntimeHandle> = Box::new(JinglerRuntimeHandleImpl {
		inner,
		instance: None,
		sample_rate: None,
	});
	Ok((rt, handle))
}
