mod native;

use anyhow::Result;

use crate::native::NativeRuntime;

pub trait JinglerRuntime: Send {
	fn load_program(&mut self, program: &ir::Program, sample_rate: f32) -> Result<()>;
	fn unload_program(&mut self);

	fn reset(&mut self);
	fn next_sample(&mut self) -> [f64; 2];
	fn note_on(&mut self, channel: u8, note: u8, velocity: u8);
	fn note_off(&mut self, channel: u8, note: u8);
	fn set_parameter(&mut self, index: usize, value: f32);
}

pub fn default_jingler_runtime() -> Box<dyn JinglerRuntime> {
	Box::new(NativeRuntime::new())
}
