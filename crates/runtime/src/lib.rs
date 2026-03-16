mod native;
mod wasm;

use anyhow::Result;

use crate::native::NativeRuntime;
use crate::wasm::WasmRuntime;

pub trait JinglerRuntime: Send {
	fn load_program(&mut self, program: &ir::Program, sample_rate: f32) -> Result<()>;
	fn unload_program(&mut self);

	fn reset(&mut self) -> Result<()>;
	fn next_sample(&mut self) -> Result<[f64; 2]>;
	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()>;
	fn note_off(&mut self, channel: u8, note: u8) -> Result<()>;
	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()>;
}

pub fn default_jingler_runtime() -> Result<Box<dyn JinglerRuntime>> {
	match std::env::var("JINGLER_RUNTIME").unwrap_or_else(|_| "wasm".to_string()).as_str() {
		"native" => NativeRuntime::new().map(|r| Box::new(r) as Box<dyn JinglerRuntime>),
		"wasm" => WasmRuntime::new().map(|r| Box::new(r) as Box<dyn JinglerRuntime>),
		_ => Err(anyhow::anyhow!("Invalid JINGLER_RUNTIME environment variable")),  
	}
}

pub struct DummyRuntime;

impl JinglerRuntime for DummyRuntime {
	fn load_program(&mut self, _program: &ir::Program, _sample_rate: f32) -> Result<()> {
		Ok(())
	}

	fn unload_program(&mut self) {}

	fn reset(&mut self) -> Result<()> {
		Ok(())
	}

	fn next_sample(&mut self) -> Result<[f64; 2]> {
		Ok([0.0, 0.0])
	}

	fn note_on(&mut self, _channel: u8, _note: u8, _velocity: u8) -> Result<()> {
		Ok(())
	}

	fn note_off(&mut self, _channel: u8, _note: u8) -> Result<()> {
		Ok(())
	}

	fn set_parameter(&mut self, _index: usize, _value: f32) -> Result<()> {
		Ok(())
	}
}
