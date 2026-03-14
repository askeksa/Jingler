use anyhow::Result;
use std::sync::{Mutex, Condvar, OnceLock};

use crate::JinglerRuntime;

static RUNTIME_SEMAPHORE: OnceLock<Semaphore> = OnceLock::new();

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe extern "C" {
	fn LoadGmDls();
	fn CompileBytecode(bytecodes: *const u8);
	fn ReleaseBytecode();
	fn ResetState();
	fn RunProcedure(constants: *const u32, proc_id: usize) -> *const [f64; 2];
	fn NoteOn(channel: i32, delta_frames: i32, key: i32, velocity: i32);
	fn NoteOff(channel: i32, delta_frames: i32, key: i32);
}

pub struct NativeRuntime {
	program: Option<NativeProgram>,
	_semaphore_guard: SemaphoreGuard,
}

struct NativeProgram {
	program: ir::Program,
	constants: Vec<u32>,
	parameter_offset: usize,
}

impl NativeRuntime {
	pub fn new() -> Result<Self> {
		let semaphore = RUNTIME_SEMAPHORE.get_or_init(|| Semaphore::new(1));
		semaphore.acquire();

		#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
		unsafe {
			LoadGmDls();
		}
		Ok(NativeRuntime { program: None, _semaphore_guard: SemaphoreGuard(semaphore) })
	}
}

impl JinglerRuntime for NativeRuntime {
	fn load_program(&mut self, program: &ir::Program, sample_rate: f32) -> Result<()> {
		self.unload_program();

		let (bytecodes, constants, parameter_offset) = ir::encode::encode_bytecodes_binary(program, sample_rate)?;

		#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
		unsafe {
			CompileBytecode(bytecodes.as_ptr());
			ResetState();
			RunProcedure(constants.as_ptr(), program.main_static_proc_id);
		}

		self.program = Some(NativeProgram {
			program: program.clone(),
			constants,
			parameter_offset,
		});

		Ok(())
	}

	fn unload_program(&mut self) {
		if self.program.take().is_some() {
			#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
			unsafe {
				ReleaseBytecode();
			}
		}
	}

	fn reset(&mut self) -> Result<()> {
		if let Some(p) = &mut self.program {
			#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
			unsafe {
				ResetState();
				RunProcedure(p.constants.as_ptr(), p.program.main_static_proc_id);
			}
		}
		Ok(())
	}

	fn next_sample(&mut self) -> Result<[f64; 2]> {
		#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
		return Ok([0.0, 0.0]);

		#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
		if let Some(p) = &mut self.program {
			unsafe {
				Ok(*RunProcedure(p.constants.as_ptr(), p.program.main_dynamic_proc_id))
			}
		} else {
			Ok([0.0, 0.0])
		}
	}

	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()> {
		if let Some(p) = &mut self.program {
			#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
			unsafe {
				for (track, track_channel) in p.program.track_order.iter().enumerate() {
					if *track_channel == channel as usize {
						NoteOn(track as i32, 0, note as i32, velocity as i32);
					}
				}
			}
		}
		Ok(())
	}

	fn note_off(&mut self, channel: u8, note: u8) -> Result<()> {
		if let Some(p) = &mut self.program {
			#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
			unsafe {
				for (track, track_channel) in p.program.track_order.iter().enumerate() {
					if *track_channel == channel as usize {
						NoteOff(track as i32, 0, note as i32);
					}
				}
			}
		}
		Ok(())
	}

	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()> {
		if let Some(p) = &mut self.program {
			if index < p.program.parameters.len() {
				let param = &p.program.parameters[index];
				let quant_value = param.min + value * (param.max - param.min);
				p.constants[p.parameter_offset + index] = quant_value.to_bits();
			}
		}
		Ok(())
	}
}

impl Drop for NativeRuntime {
	fn drop(&mut self) {
		self.unload_program();
	}
}


/// A simple semaphore using Mutex and Condvar for blocking acquisition
struct Semaphore {
	count: Mutex<u32>,
	condvar: Condvar,
}

impl Semaphore {
	fn new(initial: u32) -> Self {
		Semaphore {
			count: Mutex::new(initial),
			condvar: Condvar::new(),
		}
	}

	fn acquire(&self) {
		let mut count = self.count.lock().unwrap();
		while *count == 0 {
			count = self.condvar.wait(count).unwrap();
		}
		*count -= 1;
	}

	fn release(&self) {
		let mut count = self.count.lock().unwrap();
		*count += 1;
		self.condvar.notify_one();
	}
}

/// Guard that holds a semaphore permit and releases it when dropped
struct SemaphoreGuard(&'static Semaphore);

impl Drop for SemaphoreGuard {
	fn drop(&mut self) {
		self.0.release();
	}
}
