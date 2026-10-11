use std::io::Read;
use std::net::TcpListener;
use std::num::NonZeroU32;
use std::ops::RangeInclusive;
use std::sync::{Arc, Mutex, OnceLock};
use std::sync::Once;

use nice_plug::editor::dpi::LogicalSize;
use nice_plug::midi::{Channel, Key};
use nice_plug::prelude::*;
use nice_plug_egui::{EguiEditor, EguiEditorState, EguiNiceSettings, RepaintNotifier, create_egui_editor};
use runtime::{Externals, JinglerRuntime, JinglerRuntimeHandle, SubmitOutcome, default_jingler_runtime};

use crate::editor::JinglerEditor;
use crate::knobs::{KnobAssignment, NUM_KNOBS, format_value, normalize};
use crate::status::shared;

mod editor;
mod knobs;
mod mappings;
mod status;

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
		// No external members are implemented yet.
		match default_jingler_runtime(Externals::new()) {
			Ok((listener, handle)) => Some(Arc::new(GlobalRuntime {
				listener,
				handle: Mutex::new(handle),
			})),
			Err(e) => {
				nice_error!("Jingler: failed to create runtime: {}", e);
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

	/// The parameters the knobs are assigned to (ADR 0006).
	#[persist = "knob-assignment"]
	pub knob_assignment: Mutex<KnobAssignment>,
}

/// The knob the DAW knows as `Param <index + 1>`. Its value text shows the
/// value of the parameter it is assigned to, if any.
fn knob(index: usize) -> FloatParam {
	FloatParam::new(
		format!("Param {:02}", index + 1),
		0.5,
		FloatRange::Linear { min: 0.0, max: 1.0 },
	)
	.with_value_to_string(Arc::new(move |value| match shared().parameter(index) {
		Some(p) => format_value(p.min + value * (p.max - p.min)),
		None => format!("{value:.2}"),
	}))
	.with_string_to_value(Arc::new(move |text| {
		let value: f32 = text.trim().parse().ok()?;
		Some(match shared().parameter(index) {
			Some(p) => normalize(value, p.min, p.max),
			None => value.clamp(0.0, 1.0),
		})
	}))
}

impl Default for JinglerParams {
	fn default() -> Self {
		Self {
			p00: knob(0),
			p01: knob(1),
			p02: knob(2),
			p03: knob(3),
			p04: knob(4),
			p05: knob(5),
			p06: knob(6),
			p07: knob(7),
			p08: knob(8),
			p09: knob(9),
			p10: knob(10),
			p11: knob(11),
			p12: knob(12),
			p13: knob(13),
			p14: knob(14),
			knob_assignment: Mutex::new(KnobAssignment::default()),
		}
	}
}

impl JinglerParams {
	fn knob(&self, index: usize) -> &FloatParam {
		[
			&self.p00, &self.p01, &self.p02, &self.p03, &self.p04,
			&self.p05, &self.p06, &self.p07, &self.p08, &self.p09,
			&self.p10, &self.p11, &self.p12, &self.p13, &self.p14,
		][index]
	}
}

// ─── Plugin struct ────────────────────────────────────────────────────────────

struct JinglerPlugin {
	params: Arc<JinglerParams>,
	runtime: Option<Arc<GlobalRuntime>>,
	sample_rate: f32,
	editor_state: Arc<EguiEditorState>,
	/// Repaints the editor when a program or an error arrives.
	repaint: RepaintNotifier,
	repaint_id: u64,
}

impl Default for JinglerPlugin {
	fn default() -> Self {
		let repaint = RepaintNotifier::new();
		Self {
			params: Arc::new(JinglerParams::default()),
			runtime: global_runtime(),
			sample_rate: 44100.0,
			editor_state: EguiEditorState::from_size(LogicalSize::new(780.0, 480.0), 1.0),
			repaint_id: shared().register(repaint.clone()),
			repaint,
		}
	}
}

impl Drop for JinglerPlugin {
	fn drop(&mut self) {
		shared().unregister(self.repaint_id);
	}
}

// ─── Network listener thread ──────────────────────────────────────────────────

fn listener_thread(runtime: Arc<GlobalRuntime>) {
	let listener = match TcpListener::bind(LISTEN_ADDR) {
		Ok(l) => l,
		Err(e) => {
			nice_error!("Jingler: failed to bind {}: {}", LISTEN_ADDR, e);
			return;
		}
	};
	nice_log!("Jingler: listening for compiled programs on {}", LISTEN_ADDR);

	for stream in listener.incoming() {
		match stream {
			Ok(mut stream) => {
				// Protocol: 4-byte LE u32 length, then that many bytes of
				// bincode-serialised ir::Program.
				let mut len_buf = [0u8; 4];
				if stream.read_exact(&mut len_buf).is_err() {
					nice_error!("Jingler: failed to read length prefix");
					continue;
				}
				let len = u32::from_le_bytes(len_buf) as usize;
				if len > MAX_PROGRAM_BYTES {
					report_error(format!("Program too large ({} bytes, max {})", len, MAX_PROGRAM_BYTES));
					continue;
				}

				let mut data = vec![0u8; len];
				if stream.read_exact(&mut data).is_err() {
					report_error(format!("Failed to read {} bytes of program data", len));
					continue;
				}

				match bincode::deserialize::<ir::Program>(&data) {
					Ok(program) => {
						match runtime.listener.submit_program(&program) {
							Ok(outcome) => {
								match outcome {
									SubmitOutcome::FreshCompile => {
										nice_log!("Jingler: new program compiled ({} bytes)", len);
									}
									SubmitOutcome::ConstantUpdate { count } => {
										nice_log!("Jingler: queued {} constant update(s)", count);
									}
									SubmitOutcome::NoChange => {
										nice_log!("Jingler: program unchanged ({} bytes)", len);
									}
									_ => {}
								}
								shared().accept_program(program);
							}
							Err(e) => report_error(format!("Program rejected: {}", e)),
						}
					}
					Err(e) => report_error(format!("Failed to deserialise program: {}", e)),
				}
			}
			Err(e) => {
				report_error(format!("Failed to accept connection: {}", e));
			},
		}
	}
}

/// Log an error and show it in the editors.
fn report_error(error: String) {
	nice_error!("Jingler: {}", error);
	shared().report_error(error);
}

// ─── Note events ──────────────────────────────────────────────────────────────

/// The channel numbers a note event addresses: all 16 for a wildcard.
fn channel_numbers(channel: Channel) -> RangeInclusive<u8> {
	match channel {
		Channel::Number(channel) => channel..=channel,
		Channel::Wildcard => 0..=15,
	}
}

/// The key numbers a note event addresses: all 128 for a wildcard.
fn key_numbers(key: Key) -> RangeInclusive<u8> {
	match key {
		Key::Number(key) => key..=key,
		Key::Wildcard => 0..=127,
	}
}

// ─── Plugin implementation ────────────────────────────────────────────────────

impl JinglerPlugin {
	fn param_values(&self) -> [f32; NUM_KNOBS] {
		std::array::from_fn(|index| self.params.knob(index).value())
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

	type Editor = EguiEditor<JinglerEditor>;
	type SysExMessage = ();
	type BackgroundTask = ();

	fn params(&self) -> Arc<dyn Params> {
		self.params.clone()
	}

	fn editor(&mut self, _async_executor: AsyncExecutor<Self>) -> Option<Self::Editor> {
		let settings = EguiNiceSettings::new()
			.with_tile("Jingler")
			.with_resize_hint(ResizeHint::resizable().with_min_logical_size(LogicalSize::new(520.0, 240.0)));
		create_egui_editor(self.editor_state.clone(), self.repaint.clone(), settings, JinglerEditor::new(self.params.clone()))
	}

	/// A state saved before knob assignments were has none: adopt the next
	/// program's parameters without moving any knob (ADR 0006).
	fn filter_state(state: &mut PluginState) {
		state.fields.entry("knob-assignment".to_string())
			.or_insert_with(|| serde_json::to_string(&KnobAssignment::Unknown).unwrap());
	}

	fn activate(
		&mut self,
		_audio_io_layout: &AudioIOLayout,
		buffer_config: &BufferConfig,
		_context: &mut impl ActivateContext<Self>,
	) -> bool {
		nice_log!("Jingler: initializing");
		self.sample_rate = buffer_config.sample_rate;

		let Some(runtime) = self.runtime.clone() else {
			nice_error!("Jingler: runtime unavailable");
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
				nice_error!("Jingler: runtime error: {}", e);
				return false;
			}
		}

		true
	}

	fn deactivate(&mut self) {
		nice_log!("Jingler: deactivating");
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
						nice_error!("Jingler: runtime error in {}: {}", $where, e);
						shared().try_report_error(format!("Runtime error in {}: {}", $where, e));
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
							// A note on with a wildcard channel or key has no note to
							// trigger, so it falls through and is ignored.
							NoteEvent::NoteOn { channel: Channel::Number(channel), key: Key::Number(key), velocity, .. } => {
								check!(handle.note_on(channel, key, (velocity * 127.0) as u8), "note_on");
							}
							NoteEvent::NoteOff { channel, key, .. } | NoteEvent::Choke { channel, key, .. } => {
								for channel in channel_numbers(channel) {
									for key in key_numbers(key) {
										check!(handle.note_off(channel, key), "note_off");
									}
								}
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

nice_export_vst3!(JinglerPlugin);
