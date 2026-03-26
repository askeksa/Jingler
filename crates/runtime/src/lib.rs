mod wasm;

use anyhow::Result;

use crate::wasm::WasmRuntime;

pub trait JinglerRuntime: Send + Sync {
	fn load_program(&self, program: &ir::Program) -> Result<Box<dyn JinglerRuntimeInstance>>;
}

pub trait JinglerRuntimeInstance: Send {
	fn initialize(&mut self, sample_rate: f32) -> Result<()>;
	fn next_sample(&mut self) -> Result<[f64; 2]>;
	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()>;
	fn note_off(&mut self, channel: u8, note: u8) -> Result<()>;
	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()>;
}

pub fn default_jingler_runtime() -> Result<Box<dyn JinglerRuntime>> {
	WasmRuntime::new().map(|r| Box::new(r) as Box<dyn JinglerRuntime>)
}
