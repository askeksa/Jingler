use std::io::Read;
use std::net::TcpListener;
use std::num::NonZeroU32;
use std::sync::{Arc, Mutex, OnceLock};
use std::sync::Once;

use nih_plug::prelude::*;
use runtime::{JinglerRuntime, NativeRuntime};

const NUM_PARAMS: usize = 15;
const LISTEN_ADDR: &str = "localhost:26127";
/// Sanity cap on incoming serialised program size to prevent OOM from rogue senders.
const MAX_PROGRAM_BYTES: usize = 64 * 1024 * 1024; // 64 MB

// ─── Global listener state ────────────────────────────────────────────────────
//
// The TCP listener is spawned exactly once per process (not per plugin instance),
// so that re-instantiation by the DAW doesn't cause "address already in use" errors.
// All instances share the same pending-program slot.

static PENDING_PROGRAM: OnceLock<Arc<Mutex<Option<ir::Program>>>> = OnceLock::new();
static LISTENER_INIT: Once = Once::new();

fn global_pending() -> Arc<Mutex<Option<ir::Program>>> {
	Arc::clone(PENDING_PROGRAM.get_or_init(|| Arc::new(Mutex::new(None))))
}

// ─── Parameters ──────────────────────────────────────────────────────────────

#[derive(Params)]
struct JinglerParams {
	#[id = "p00"] pub p00: FloatParam,
	#[id = "p01"] pub p01: FloatParam,
	#[id = "p02"] pub p02: FloatParam,
	#[id = "p03"] pub p03: FloatParam,
	#[id = "p04"] pub p04: FloatParam,
	#[id = "p05"] pub p05: FloatParam,
	#[id = "p06"] pub p06: FloatParam,
	#[id = "p07"] pub p07: FloatParam,
	#[id = "p08"] pub p08: FloatParam,
	#[id = "p09"] pub p09: FloatParam,
	#[id = "p10"] pub p10: FloatParam,
	#[id = "p11"] pub p11: FloatParam,
	#[id = "p12"] pub p12: FloatParam,
	#[id = "p13"] pub p13: FloatParam,
	#[id = "p14"] pub p14: FloatParam,
}

impl Default for JinglerParams {
	fn default() -> Self {
		let param = |name: &str| FloatParam::new(
			name,
			0.5,
			FloatRange::Linear { min: 0.0, max: 1.0 },
		);
		Self {
			p00: param("Param 01"),
			p01: param("Param 02"),
			p02: param("Param 03"),
			p03: param("Param 04"),
			p04: param("Param 05"),
			p05: param("Param 06"),
			p06: param("Param 07"),
			p07: param("Param 08"),
			p08: param("Param 09"),
			p09: param("Param 10"),
			p10: param("Param 11"),
			p11: param("Param 12"),
			p12: param("Param 13"),
			p13: param("Param 14"),
			p14: param("Param 15"),
		}
	}
}

// ─── Plugin struct ────────────────────────────────────────────────────────────

struct JinglerPlugin {
	params: Arc<JinglerParams>,
	/// Parameter metadata from the currently loaded program. Audio-thread only.
	zing_params: Vec<ir::Parameter>,
	runtime: NativeRuntime,
	/// Stored so the program can be reloaded when the sample rate changes.
	program: Option<ir::Program>,
	/// Shared with the global listener thread; checked each audio callback.
	pending_program: Arc<Mutex<Option<ir::Program>>>,
	sample_rate: f32,
}

impl Default for JinglerPlugin {
	fn default() -> Self {
		Self {
			params: Arc::new(JinglerParams::default()),
			zing_params: vec![],
			runtime: NativeRuntime::new(),
			program: None,
			pending_program: global_pending(),
			sample_rate: 44100.0,
		}
	}
}

// ─── Network listener thread ──────────────────────────────────────────────────

fn listener_thread(pending: Arc<Mutex<Option<ir::Program>>>) {
	let listener = match TcpListener::bind(LISTEN_ADDR) {
		Ok(l) => l,
		Err(e) => {
			nih_error!("Jingler: failed to bind {}: {}", LISTEN_ADDR, e);
			return;
		}
	};
	nih_log!("Jingler: listening for compiled programs on {}", LISTEN_ADDR);

	for stream in listener.incoming() {
		match stream {
			Ok(mut stream) => {
				// Protocol: 4-byte LE u32 length, then that many bytes of
				// bincode-serialised ir::Program.
				let mut len_buf = [0u8; 4];
				if stream.read_exact(&mut len_buf).is_err() {
					nih_error!("Jingler: failed to read length prefix");
					continue;
				}
				let len = u32::from_le_bytes(len_buf) as usize;
				if len > MAX_PROGRAM_BYTES {
					nih_error!("Jingler: program too large ({} bytes, max {})", len, MAX_PROGRAM_BYTES);
					continue;
				}

				let mut data = vec![0u8; len];
				if stream.read_exact(&mut data).is_err() {
					nih_error!("Jingler: failed to read {} bytes of program data", len);
					continue;
				}

				match bincode::deserialize::<ir::Program>(&data) {
					Ok(program) => {
						*pending.lock().unwrap() = Some(program);
						nih_log!("Jingler: new program received ({} bytes)", len);
					}
					Err(e) => nih_error!("Jingler: deserialise error: {}", e),
				}
			}
			Err(e) => nih_error!("Jingler: accept error: {}", e),
		}
	}
}

// ─── Plugin implementation ────────────────────────────────────────────────────

impl JinglerPlugin {
	fn load_program(&mut self, program: ir::Program) {
		self.runtime.unload_program();
		self.zing_params = program.parameters.clone();
		match self.runtime.load_program(program.clone(), self.sample_rate) {
			Ok(_) => self.program = Some(program),
			Err(e) => nih_error!("Jingler: runtime error: {}", e),
		}
	}

	fn param_values(&self) -> [f32; NUM_PARAMS] {
		[
			self.params.p00.value(),
			self.params.p01.value(),
			self.params.p02.value(),
			self.params.p03.value(),
			self.params.p04.value(),
			self.params.p05.value(),
			self.params.p06.value(),
			self.params.p07.value(),
			self.params.p08.value(),
			self.params.p09.value(),
			self.params.p10.value(),
			self.params.p11.value(),
			self.params.p12.value(),
			self.params.p13.value(),
			self.params.p14.value(),
		]
	}
}

impl Plugin for JinglerPlugin {
	const NAME: &'static str = "Jingler";
	const VENDOR: &'static str = "Loonies";
	const URL: &'static str = "";
	const EMAIL: &'static str = "";
	const VERSION: &'static str = env!("CARGO_PKG_VERSION");

	const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[AudioIOLayout {
		main_input_channels: None,
		main_output_channels: NonZeroU32::new(2),
		..AudioIOLayout::const_default()
	}];

	const MIDI_INPUT: MidiConfig = MidiConfig::Basic;
	const SAMPLE_ACCURATE_AUTOMATION: bool = true;

	type SysExMessage = ();
	type BackgroundTask = ();

	fn params(&self) -> Arc<dyn Params> {
		self.params.clone()
	}

	fn initialize(
		&mut self,
		_audio_io_layout: &AudioIOLayout,
		buffer_config: &BufferConfig,
		_context: &mut impl InitContext<Self>,
	) -> bool {
		self.sample_rate = buffer_config.sample_rate;

		// Spawn the TCP listener thread exactly once for the whole process lifetime,
		// so DAW re-instantiation doesn't cause "address already in use" errors.
		LISTENER_INIT.call_once(|| {
			let pending = global_pending();
			std::thread::Builder::new()
				.name("jingler-listener".into())
				.spawn(move || listener_thread(pending))
				.expect("failed to spawn Jingler listener thread");
		});

		// If a program was already loaded, reload it at the new sample rate.
		if let Some(program) = self.program.take() {
			self.load_program(program);
		}

		true
	}

	fn deactivate(&mut self) {
		self.runtime.unload_program();
		self.program = None;
	}

	fn process(
		&mut self,
		buffer: &mut Buffer,
		_aux: &mut AuxiliaryBuffers,
		context: &mut impl ProcessContext<Self>,
	) -> ProcessStatus {
		// Hot-swap program if a new one arrived over the network.
		// Use try_lock so the audio thread never blocks waiting for the listener.
		let new_program = self.pending_program.try_lock().ok().and_then(|mut g| g.take());
		if let Some(program) = new_program {
			self.load_program(program);
		}

		// Push normalised (0–1) parameter values to the runtime.
		// The runtime scales them to the Zing parameter range internally.
		let values = self.param_values();
		for (i, &v) in values.iter().enumerate() {
			self.runtime.set_parameter(i, v);
		}

		// Process audio sample-by-sample, interleaving MIDI events at their
		// correct sample offsets.
		let mut next_event = context.next_event();
		for (sample_id, channel_samples) in buffer.iter_samples().enumerate() {
			// Dispatch all events scheduled at or before this sample.
			loop {
				match next_event {
					Some(ref event) if event.timing() <= sample_id as u32 => {
						match *event {
							NoteEvent::NoteOn { channel, note, velocity, .. } => {
								self.runtime.note_on(channel, note, (velocity * 127.0) as u8);
							}
							NoteEvent::NoteOff { channel, note, .. } => {
								self.runtime.note_off(channel, note);
							}
							NoteEvent::Choke { channel, note, .. } => {
								self.runtime.note_off(channel, note);
							}
							_ => {}
						}
						next_event = context.next_event();
					}
					_ => break,
				}
			}

			let [left, right] = self.runtime.next_sample();
			let mut samples = channel_samples.into_iter();
			if let Some(l) = samples.next() { *l = left as f32; }
			if let Some(r) = samples.next() { *r = right as f32; }
		}

		ProcessStatus::Normal
	}
}

impl Vst3Plugin for JinglerPlugin {
	const VST3_CLASS_ID: [u8; 16] = *b"JinglerVstPlugin";
	const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
		&[Vst3SubCategory::Instrument, Vst3SubCategory::Synth];
}

nih_export_vst3!(JinglerPlugin);
