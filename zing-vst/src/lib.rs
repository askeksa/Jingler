
use bytecode::bytecodes::Bytecode;
use bytecode::encode::encode_bytecodes;
use zing::compiler;

use std::fs;
use std::slice;
use std::sync::mpsc::{channel, Receiver};
use std::time::Duration;

use nfd::{open_file_dialog, Response};
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use vst::buffer::AudioBuffer;
use vst::plugin::{Category, HostCallback, Info, Plugin};
use vst::plugin_main;

#[link(name = "clinklang_cmd")]
extern "C" {
	fn CompileBytecode(bytecodes: *const u8);
	fn ReleaseBytecode();
	fn RunStaticCode(constants: *const u32);
    fn RenderSamples(constants: *const u32, length: usize) -> *mut f32;
}

struct ZingPlugin {
	sample_rate: f32,
	zing_filename: String,
	watcher: RecommendedWatcher,
	watcher_receiver: Receiver<DebouncedEvent>,
	program: Vec<Bytecode>,
	constants: Vec<u32>,
	bytecode_compiled: bool,
}

impl Default for ZingPlugin {
	fn default() -> Self {
		let (tx, rx) = channel();
		let watcher = watcher(tx, Duration::from_secs_f32(0.1)).unwrap();
		ZingPlugin {
			sample_rate: 44100.0,
			zing_filename: "".to_string(),
			watcher,
			watcher_receiver: rx,
			bytecode_compiled: false,
			program: vec![],
			constants: vec![],
		}
	}
}

impl ZingPlugin {
	fn compile(&mut self) {
		self.release_program();
		match fs::read_to_string(&self.zing_filename) {
			Ok(s) => match compiler::Compiler::new(&self.zing_filename, &s).compile() {
				Ok(program) => {
					self.program = program;
					self.init_program();
				},
				Err(errors) => {
					let message = errors.into_iter().collect::<String>();
					simple_message_box::create_message_box(&message, "Error");
				},
			},
			Err(e) => {
				let message = format!("Error reading '{}': {}", self.zing_filename, e);
				simple_message_box::create_message_box(&message, "Error");
			},
		}
	}

	fn init_program(&mut self) {
		debug_assert!(!self.bytecode_compiled);
		match encode_bytecodes(&self.program, self.sample_rate) {
			Ok((bytecodes, constants)) => unsafe {
				CompileBytecode(bytecodes.as_ptr());
				self.bytecode_compiled = true;

				RunStaticCode(constants.as_ptr());
				self.constants = constants;
			},
			Err(message) => {
				let message = format!("Encoding error: {}", message);
				simple_message_box::create_message_box(&message, "Error");
			},
		}
	}

	fn release_program(&mut self) {
		if self.bytecode_compiled {
			unsafe {
				ReleaseBytecode();
				self.bytecode_compiled = false;
			}
		}
	}
}

impl Plugin for ZingPlugin {
	fn new(_host: HostCallback) -> ZingPlugin {
		loop {
			let filename = loop {
				if let Ok(Response::Okay(path)) = open_file_dialog(None, None) {
					break path;
				}
			};
			let mut plugin = ZingPlugin::default();
			plugin.zing_filename = filename.to_string();
			if let Ok(()) = plugin.watcher.watch(&plugin.zing_filename, RecursiveMode::NonRecursive) {
				plugin.compile();
				return plugin;
			}
		}
	}

	fn get_info(&self) -> Info {
		Info {
			presets: 1,
			parameters: 0,
			inputs: 0,
			outputs: 2,
			category: Category::Synth,
			f64_precision: false,
			preset_chunks: false,

			name: "ClinklangZing".to_string(),
			vendor: "Loonies".to_string(),
			unique_id: 0xDEAF,
			version: 100,

			.. Info::default()
		}
	}

	fn set_sample_rate(&mut self, sample_rate: f32) {
		self.sample_rate = sample_rate;
		self.release_program();
		self.init_program();
	}

	fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
		let length = buffer.samples();
		let (_, mut out) = buffer.split();
		if let Ok(DebouncedEvent::Write(_)) = self.watcher_receiver.try_recv() {
			self.compile();
		}
		if self.bytecode_compiled {
			let rendered = unsafe {
				let samples = RenderSamples(self.constants.as_ptr(), length);
				slice::from_raw_parts(samples, length * 2)
			};
			for i in 0..length {
				out[0][i] = rendered[i * 2 + 0];
				out[1][i] = rendered[i * 2 + 1];
			}
		}
	}
}

impl Drop for ZingPlugin {
	fn drop(&mut self) {
		self.release_program();
	}
}

plugin_main!(ZingPlugin);
