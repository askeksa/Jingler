
use bytecode::bytecodes::Bytecode;
use bytecode::encode::encode_bytecodes;
use zing::compiler;

use std::collections::{VecDeque};
use std::fs;
use std::slice;
use std::sync::mpsc::{channel, Receiver};
use std::time::Duration;

use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use rfd::FileDialog;
use vst::api::Events;
use vst::buffer::AudioBuffer;
use vst::event::{Event, MidiEvent};
use vst::plugin::{Category, HostCallback, Info, Plugin};
use vst::plugin_main;

#[link(name = "clinklang_cmd")]
extern "C" {
	fn CompileBytecode(bytecodes: *const u8);
	fn ReleaseBytecode();
	fn RunStaticCode(constants: *const u32);
    fn RenderSamples(constants: *const u32, length: usize) -> *mut f32;
    fn NoteOn(channel: u32, delta_frames: i32, key: u32, velocity: u32);
    fn NoteOff(channel: u32, delta_frames: i32, key: u32);
}

struct ZingPlugin {
	sample_rate: f32,
	time: i32,
	events: VecDeque<MidiEvent>,

	zing_filename: String,
	watcher: RecommendedWatcher,
	watcher_receiver: Receiver<DebouncedEvent>,
	program: Option<Vec<Bytecode>>,
	constants: Vec<u32>,
	bytecode_compiled: bool,
}

impl Default for ZingPlugin {
	fn default() -> Self {
		let (tx, rx) = channel();
		let watcher = watcher(tx, Duration::from_secs_f32(0.1)).unwrap();
		ZingPlugin {
			sample_rate: 44100.0,
			time: 0,
			events: VecDeque::new(),
			zing_filename: "".to_string(),
			watcher,
			watcher_receiver: rx,
			bytecode_compiled: false,
			program: None,
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
					self.program = Some(program);
					self.init_program();
				},
				Err(errors) => {
					self.program = None;
					let message = errors.into_iter().collect::<String>();
					simple_message_box::create_message_box(&message, "Error");
				},
			},
			Err(e) => {
				self.program = None;
				let message = format!("Error reading '{}': {}", self.zing_filename, e);
				simple_message_box::create_message_box(&message, "Error");
			},
		}
	}

	fn init_program(&mut self) {
		debug_assert!(!self.bytecode_compiled);
		if let Some(ref program) = self.program {
			match encode_bytecodes(program, self.sample_rate) {
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
				if let Some(path) = FileDialog::new().pick_file() {
					break path;
				}
			};
			let mut plugin = ZingPlugin::default();
			plugin.zing_filename = filename.to_string_lossy().into_owned();
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

	fn process_events(&mut self, events: &Events) {
		for event in events.events() {
			if let Event::Midi(mut midi) = event {
				midi.delta_frames += self.time;
				self.events.push_back(midi);
			}
		}
	}

	fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
		let end_time = self.time + buffer.samples() as i32;
		let (_, mut out) = buffer.split();
		if let Ok(DebouncedEvent::Write(_)) = self.watcher_receiver.try_recv() {
			self.compile();
		}
		if self.bytecode_compiled {
			let mut base = 0usize;
			while self.time < end_time {
				let mut dirty = true;
				let mut next_time = self.events.front().map(|m| m.delta_frames).unwrap_or(end_time);
				while next_time <= self.time {
					let data = self.events.pop_front().unwrap().data;
					let channel = (data[0] & 0x0F) as u32;
					let key = data[1] as u32;
					let velocity = data[2] as u32;
					match data[0] & 0xF0 {
						0x90 => unsafe {
							// Note On
							NoteOn(channel, 0, key, velocity);
							dirty = true;
						},
						0x80 => unsafe {
							// Note Off
							NoteOff(channel, 0, key);
						},
						0xB0 if data[1] == 120 => {
							// All sound off
							if dirty {
								self.release_program();
								self.init_program();
								dirty = false;
							}
						},
						_ => {},
					}
					next_time = self.events.front().map(|m| m.delta_frames).unwrap_or(end_time);
				}
				let length = (next_time - self.time) as usize;
				let rendered = unsafe {
					let samples = RenderSamples(self.constants.as_ptr(), length);
					slice::from_raw_parts(samples, length * 2)
				};
				for i in 0..length {
					out[0][base + i] = rendered[i * 2 + 0];
					out[1][base + i] = rendered[i * 2 + 1];
				}
				base += length;
				self.time = next_time;
			}
		} else {
			self.time = end_time;
		}
	}
}

impl Drop for ZingPlugin {
	fn drop(&mut self) {
		self.release_program();
	}
}

plugin_main!(ZingPlugin);
