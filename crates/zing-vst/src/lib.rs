use std::collections::VecDeque;
use std::fs;
use std::ops::DerefMut;
use std::sync::{Arc, RwLock};
use std::sync::mpsc::{channel, Receiver};
use std::time::Duration;

use runtime::{JinglerRuntime, NativeRuntime};
use zing::compiler;

use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use rfd::FileDialog;
use vst::api::Events;
use vst::buffer::AudioBuffer;
use vst::event::{Event, MidiEvent};
use vst::plugin::{Category, HostCallback, Info, Plugin, PluginParameters};
use vst::plugin_main;
use vst::util::AtomicFloat;

const NUM_PARAMETERS: usize = 15;

struct ZingPlugin {
	sample_rate: f32,
	time: i32,
	events: VecDeque<MidiEvent>,

	zing_filename: String,
	watcher: RecommendedWatcher,
	watcher_receiver: Receiver<DebouncedEvent>,

	runtime: NativeRuntime,
	program: Option<ir::Program>,

	parameters: Arc<ZingParameters>,
}

#[derive(Default)]
struct ZingParameters {
	zing_parameters: RwLock<Vec<ir::Parameter>>,
	values: [AtomicFloat; NUM_PARAMETERS],
}

impl ZingParameters {
	fn update_zing(&self, new: &Vec<ir::Parameter>) {
		let new = new.clone();
		let mut new_values = [0f32; NUM_PARAMETERS];
		let mut old = self.zing_parameters.write().unwrap();
		for i in 0..NUM_PARAMETERS {
			new_values[i] = if i < new.len() {
				let np = &new[i];
				if let Some(old_index) = old.iter().position(|p| p.name == np.name) {
					self.values[old_index].get()
				} else {
					((np.default - np.min) / (np.max - np.min)).max(0.0).min(1.0)
				}
			} else {
				0.0
			}
		}
		for i in 0..NUM_PARAMETERS {
			self.values[i].set(new_values[i]);
		}
		*old.deref_mut() = new;
	}
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

			runtime: NativeRuntime::new(),
			program: None,

			parameters: Arc::new(ZingParameters::default()),
		}
	}
}

impl ZingPlugin {
	fn compile(&mut self) {
		self.release_program();
		match fs::read_to_string(&self.zing_filename) {
			Ok(s) => match compiler::Compiler::new(self.zing_filename.to_string(), s).compile() {
				Ok(program) => {
					self.parameters.update_zing(&program.parameters);
					self.init_program(program);
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

	fn init_program(&mut self, program: ir::Program) {
		match self.runtime.load_program(program.clone(), self.sample_rate) {
			Ok(_) => self.program = Some(program),
			Err(e) => {
				let message = format!("Runtime error: {}", e);
				simple_message_box::create_message_box(&message, "Error");
			}
		}
	}

	fn release_program(&mut self) {
		self.runtime.unload_program();
		self.program = None;
	}
}

impl Plugin for ZingPlugin {
	fn new(_host: HostCallback) -> ZingPlugin {
		let mut plugin = ZingPlugin::default();
		loop {
			let filename = loop {
				if let Some(path) = FileDialog::new().pick_file() {
					break path;
				}
			};
			let filename = filename.to_string_lossy().into_owned();
			if let Ok(()) = plugin.watcher.watch(&filename, RecursiveMode::NonRecursive) {
				plugin.zing_filename = filename;
				plugin.compile();
				return plugin;
			}
		}
	}

	fn get_info(&self) -> Info {
		Info {
			presets: 1,
			parameters: NUM_PARAMETERS as i32,
			inputs: 0,
			outputs: 2,
			category: Category::Synth,
			f64_precision: false,
			preset_chunks: false,

			name: "JinglerZing".to_string(),
			vendor: "Loonies".to_string(),
			unique_id: 0xDEAF,
			version: 100,

			.. Info::default()
		}
	}

	fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
		self.parameters.clone()
	}

	fn set_sample_rate(&mut self, sample_rate: f32) {
		self.sample_rate = sample_rate;
		if let Some(program) = self.program.clone() {
			self.init_program(program);
		}
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

		for i in 0..NUM_PARAMETERS {
			self.runtime.set_parameter(i, self.parameters.values[i].get());
		}

		let mut base = 0usize;
		while self.time < end_time {
			let mut dirty = true;
			let mut next_time = self.events.front().map(|m| m.delta_frames).unwrap_or(end_time);
			while next_time <= self.time {
				let data = self.events.pop_front().unwrap().data;
				let channel = (data[0] & 0x0F) as u8;
				let key = data[1] as u8;
				let velocity = data[2] as u8;
				match data[0] & 0xF0 {
					0x90 => {
						// Note On
						self.runtime.note_on(channel, key, velocity);
						dirty = true;
					},
					0x80 => {
						// Note Off
						self.runtime.note_off(channel, key);
					},
					0xB0 if data[1] == 120 => {
						// All sound off
						if dirty {
							self.runtime.reset();
							dirty = false;
						}
					},
					_ => {},
				}
				next_time = self.events.front().map(|m| m.delta_frames).unwrap_or(end_time);
			}
			let length = (next_time - self.time) as usize;
			for i in 0..length {
				let [left, right] = self.runtime.next_sample();
				out[0][base + i] = left as f32;
				out[1][base + i] = right as f32;
			}
			base += length;
			self.time = next_time;
		}
	}
}

impl Drop for ZingPlugin {
	fn drop(&mut self) {
		self.release_program();
	}
}

impl PluginParameters for ZingParameters {
	fn get_parameter(&self, index: i32) -> f32 {
		let index = index as usize;
		self.values[index].get()
	}

	fn set_parameter(&self, index: i32, value: f32) {
		let index = index as usize;
		self.values[index].set(value);
	}

	fn get_parameter_name(&self, index: i32) -> String {
		let index = index as usize;
		let zing_parameters = self.zing_parameters.read().unwrap();
		if index < zing_parameters.len() {
			zing_parameters[index].name.clone()
		} else {
			"".to_string()
		}
	}

	fn get_parameter_text(&self, index: i32) -> String {
		let index = index as usize;
		let value = self.values[index].get();
		let zing_parameters = self.zing_parameters.read().unwrap();
		let display = if index < zing_parameters.len() {
			let p = &zing_parameters[index];
			p.min + value * (p.max - p.min)
		} else {
			value
		};
		format!("{display}")
	}
}

plugin_main!(ZingPlugin);
