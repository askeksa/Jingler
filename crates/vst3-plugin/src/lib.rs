use std::io::Read;
use std::net::TcpListener;
use std::num::NonZeroU32;
use std::sync::{Arc, Mutex, OnceLock};
use std::sync::Once;

use nih_plug::prelude::*;
use runtime::{JinglerRuntime, JinglerRuntimeHandle, SubmitOutcome, default_jingler_runtime};

const NUM_PARAMS: usize = 15;
const LISTEN_ADDR: &str = "0.0.0.0:26127";
/// Sanity cap on incoming serialised program size to prevent OOM from rogue senders.
const MAX_PROGRAM_BYTES: usize = 64 * 1024 * 1024; // 64 MB

// ─── Global runtime + listener state ─────────────────────────────────────────
//
// The TCP listener is spawned exactly once per process (not per plugin instance),
// so that re-instantiation by the DAW doesn't cause "address already in use"
// errors. All plugin instances share the same runtime (which owns the staged
// program / pending constant updates) and the audio-side handle.

struct GlobalRuntime {
	listener: Arc<dyn JinglerRuntime>,
	handle: Mutex<Box<dyn JinglerRuntimeHandle>>,
}

static GLOBAL_RUNTIME: OnceLock<Option<Arc<GlobalRuntime>>> = OnceLock::new();
static LISTENER_INIT: Once = Once::new();

fn global_runtime() -> Option<Arc<GlobalRuntime>> {
	GLOBAL_RUNTIME.get_or_init(|| {
		match default_jingler_runtime() {
			Ok((listener, handle)) => Some(Arc::new(GlobalRuntime {
				listener,
				handle: Mutex::new(handle),
			})),
			Err(e) => {
				nih_error!("Jingler: failed to create runtime: {}", e);
				None
			}
		}
	}).clone()
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
	runtime: Option<Arc<GlobalRuntime>>,
	sample_rate: f32,
}

impl Default for JinglerPlugin {
	fn default() -> Self {
		Self {
			params: Arc::new(JinglerParams::default()),
			runtime: global_runtime(),
			sample_rate: 44100.0,
		}
	}
}

// ─── Network listener thread ──────────────────────────────────────────────────

fn listener_thread(runtime: Arc<GlobalRuntime>) {
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
						match runtime.listener.submit_program(&program) {
							Ok(SubmitOutcome::FreshCompile) => {
								nih_log!("Jingler: new program compiled ({} bytes)", len);
							}
							Ok(SubmitOutcome::ConstantUpdate { count }) => {
								nih_log!("Jingler: queued {} constant update(s)", count);
							}
							Ok(SubmitOutcome::NoChange) => {
								nih_log!("Jingler: program unchanged ({} bytes)", len);
							}
							Ok(_) => {}
							Err(e) => nih_error!("Jingler: runtime error: {}", e),
						}
					}
					Err(e) => nih_error!("Jingler: deserialise error: {}", e),
				}
			}
			Err(e) => {
				nih_error!("Jingler: accept error: {}", e);
			},
		}
	}
}

// ─── Plugin implementation ────────────────────────────────────────────────────

impl JinglerPlugin {
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
		nih_log!("Jingler: initializing");
		self.sample_rate = buffer_config.sample_rate;

		let Some(runtime) = self.runtime.clone() else {
			nih_error!("Jingler: runtime unavailable");
			return false;
		};

		// Spawn the TCP listener thread exactly once for the whole process lifetime,
		// so DAW re-instantiation doesn't cause "address already in use" errors.
		LISTENER_INIT.call_once(|| {
			let rt_for_thread = runtime.clone();
			std::thread::Builder::new()
				.name("jingler-listener".into())
				.spawn(move || listener_thread(rt_for_thread))
				.expect("failed to spawn Jingler listener thread");
		});

		// Cache the sample rate on the audio handle. Any already-installed
		// instance is re-initialized; freshly installed ones (via poll_pending)
		// will pick up the cached rate.
		if let Ok(mut handle) = runtime.handle.lock() {
			if let Err(e) = handle.initialize(self.sample_rate) {
				nih_error!("Jingler: runtime error: {}", e);
				return false;
			}
		}

		true
	}

	fn deactivate(&mut self) {
		nih_log!("Jingler: deactivating");
	}

	fn process(
		&mut self,
		buffer: &mut Buffer,
		_aux: &mut AuxiliaryBuffers,
		context: &mut impl ProcessContext<Self>,
	) -> ProcessStatus {
		macro_rules! check {
			($action:expr, $where:expr) => {
				match $action {
					Ok(result) => result,
					Err(e) => {
						nih_error!("Jingler: runtime error in {}: {}", $where, e);
						return ProcessStatus::Error(concat!("Runtime error in ", $where));
					}
				}
			};
		}

		// Get current parameter values.
		let parameter_values = self.param_values();

		let Some(runtime) = self.runtime.as_ref() else {
			for channel_samples in buffer.iter_samples() {
				for sample in channel_samples {
					*sample = 0.0;
				}
			}
			return ProcessStatus::Normal;
		};

		// The handle lives behind a Mutex shared across plugin instances; the
		// audio thread takes the lock for the duration of this buffer. The
		// listener thread never holds it.
		let Ok(mut handle) = runtime.handle.lock() else {
			return ProcessStatus::Error("handle mutex poisoned");
		};

		// Drain any work staged by the listener thread: install a freshly
		// compiled instance and/or apply queued constant updates.
		check!(handle.poll_pending(), "poll_pending");

		// Push normalised (0–1) parameter values to the runtime.
		for (i, &v) in parameter_values.iter().enumerate() {
			check!(handle.set_parameter(i, v), "set_parameter");
		}

		// Process audio sample-by-sample, interleaving MIDI events at their
		// correct sample offsets.
		let mut next_event = context.next_event();
		for (sample_id, channel_samples) in buffer.iter_samples().enumerate() {
			loop {
				match next_event {
					Some(ref event) if event.timing() <= sample_id as u32 => {
						match *event {
							NoteEvent::NoteOn { channel, note, velocity, .. } => {
								check!(handle.note_on(channel, note, (velocity * 127.0) as u8), "note_on");
							}
							NoteEvent::NoteOff { channel, note, .. } => {
								check!(handle.note_off(channel, note), "note_off");
							}
							NoteEvent::Choke { channel, note, .. } => {
								check!(handle.note_off(channel, note), "note_off");
							}
							_ => {}
						}
						next_event = context.next_event();
					}
					_ => break,
				}
			}

			let [left, right] = check!(handle.next_sample(), "next_sample");
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
