use convert::{Music, convert_music_with_program, renoise::convert_renoise_file};
use runtime::{JinglerRuntimeHandle, default_jingler_runtime};
use zing::compiler;

use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::net::TcpStream;
use std::num::NonZero;
use std::path::PathBuf;
use std::sync::mpsc::channel;
use std::time::{Duration};

use chrono::Local;
use clap::Parser;
use hound::{SampleFormat, WavSpec, WavWriter};
use notify_debouncer_mini::notify::RecursiveMode;
use notify_debouncer_mini::{new_debouncer, DebounceEventResult};

use rodio::buffer::SamplesBuffer;
use rodio::{DeviceSinkBuilder, Player};

const DEFAULT_CONNECT_ADDR: &str = "127.0.0.1:26127";

#[derive(Parser)]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct PlayOptions {
	/// Zing file to convert/play.
	zing_file: String,

	/// Renoise file containing music to convert/play.
	#[arg(short, long, value_name = "RENOISE_FILE", help_heading = "Input options")]
	xrns: Option<String>,

	/// Stay resident and reload file when it changes.
	#[arg(short, long)]
	resident: bool,

	/// Pretty-print program to stdout.
	#[arg(short, long, help_heading = "Code output")]
	pretty_print: bool,

	/// Print IR to stdout.
	#[arg(short = 'i', long, help_heading = "Code output")]
	print_ir: bool,

	/// Write generated Wasm to file.
	#[arg(short = 'g', long, value_name = "WASM_FILE", help_heading = "Code output")]
	write_wasm: Option<String>,

	/// Send the program to a listening plugin.
	#[arg(short, long, help_heading = "Code output")]
	connect: bool,

	/// Write asm source file for music playback.
	#[arg(short = 'o', long, value_name = "OUTPUT_FILE", help_heading = "Code output")]
	write_source: Option<String>,

	/// Play audio.
	#[arg(short = 'y', long, help_heading = "Audio output")]
	play: bool,

	/// Write WAV file.
	#[arg(short = 'w', long, value_name = "WAV_FILE", help_heading = "Audio output")]
	write_wav: Option<String>,

	/// Address and port to connect to.
	#[arg(short, long, value_name = "ADDRESS", default_value = DEFAULT_CONNECT_ADDR, help_heading = "Network output options")]
	address: String,

	/// Path to jingler.asm file.
	#[arg(short, long, value_name = "JINGLER_ASM", default_value = "jingler.asm", help_heading = "Source output options")]
	jingler_asm_path: String,

	/// Use separate index byte for constant instructions.
	#[arg(short, long, help_heading = "Source output options")]
	byte_index: bool,

	/// Number of quantization levels for parameters.
	#[arg(short, long, value_name = "QUANTIZATION", default_value_t = 16, help_heading = "Source output options")]
	quantization_levels: u16,

	/// Sample rate to play at.
	#[arg(short, long, value_name = "SAMPLE_RATE", default_value_t = 44100.0, help_heading = "Audio output options")]
	sample_rate: f32,

	/// Duration of audio, in seconds.
	#[arg(short, long, value_name = "DURATION", default_value_t = 1.0, help_heading = "Audio output options")]
	duration: f32,
}


fn write_wav(filename: &str, sample_rate: f32, data: &[f32]) -> Result<(), hound::Error> {
	let spec = WavSpec {
		channels: 2,
		sample_rate: sample_rate as u32,
		bits_per_sample: 32,
		sample_format: SampleFormat::Float,
	};
	let mut writer = WavWriter::create(filename, spec)?;
	for &sample in data {
		writer.write_sample(sample)?;
	}
	writer.finalize()
}

fn play_sound(sample_rate: f32, data: &[f32]) -> Result<(), String> {
	let mut device_sink = DeviceSinkBuilder::open_default_sink()
		.map_err(|e| format!("Could not open default device: {e}"))?;
	device_sink.log_on_drop(false);
	let player = Player::connect_new(&device_sink.mixer());
	let buffer = SamplesBuffer::new(
		NonZero::new(2u16).unwrap(),
		NonZero::new(sample_rate as u32).unwrap(),
		data,
	);
	player.append(buffer);
	player.sleep_until_end();
	Ok(())
}

fn send_program(program: &ir::Program, address: &str) -> Result<(), Box<dyn Error>> {
	let mut stream = TcpStream::connect(address)?;
	let data = bincode::serialize(program)?;
	let len = data.len() as u32;
	stream.write_all(&len.to_le_bytes())?;
	stream.write_all(&data)?;
	stream.flush()?;
	println!("Sent program to {} ({} bytes)", address, data.len());
	Ok(())
}

enum MusicEvent {
	NoteOn { channel: u8, key: u8, velocity: u8 },
	NoteOff { channel: u8, key: u8 },
}

struct ParameterCurve {
	index: usize,
	points: Vec<(usize, f32)>, // (sample position, value), sorted by sample position
}

fn build_music_events(music: &Music, sample_rate: f32) -> Vec<(usize, MusicEvent)> {
	let sps = music.ticklength * sample_rate; // samples per tick/line
	let mut events: Vec<(usize, MusicEvent)> = Vec::new();

	for note in &music.notes {
		let channel = music.instruments.get(note.instr as usize)
			.map(|inst| inst.channel as u8)
			.unwrap_or(0);

		let on_sample = (note.line as f32 * sps) as usize;
		events.push((on_sample, MusicEvent::NoteOn {
			channel,
			key: note.key as u8,
			velocity: note.velocity as u8,
		}));

		let length = note.length.unwrap_or(0x7E00);
		let off_sample = ((note.line + length) as f32 * sps) as usize;
		events.push((off_sample, MusicEvent::NoteOff {
			channel,
			key: note.key as u8,
		}));
	}

	// Within the same sample: note-offs before note-ons
	events.sort_by_key(|(sample, event)| {
		let priority = match event {
			MusicEvent::NoteOff { .. } => 0,
			MusicEvent::NoteOn { .. } => 1,
		};
		(*sample, priority)
	});

	events
}

fn build_parameter_curves(music: &Music, sample_rate: f32) -> Vec<ParameterCurve> {
	let sps = music.ticklength * sample_rate;
	music.autos.iter().enumerate()
		.filter(|(_, points)| !points.is_empty())
		.map(|(param_idx, points)| ParameterCurve {
			index: param_idx,
			points: points.iter()
				.map(|p| ((p.line as f32 * sps) as usize, p.value))
				.collect(),
		})
		.collect()
}

fn interpolate_curve(points: &[(usize, f32)], sample: usize, cursor: &mut usize) -> f32 {
	if sample <= points[0].0 { return points[0].1; }
	while *cursor + 1 < points.len() && points[*cursor + 1].0 <= sample {
		*cursor += 1;
	}
	if *cursor + 1 >= points.len() { return points[*cursor].1; }
	let (s0, v0) = points[*cursor];
	let (s1, v1) = points[*cursor + 1];
	let t = (sample - s0) as f32 / (s1 - s0) as f32;
	v0 + t * (v1 - v0)
}

fn compute_audio(
	instance: &mut dyn JinglerRuntimeHandle,
	sample_rate: f32,
	n_samples: usize,
	events: &[(usize, MusicEvent)],
	curves: &[ParameterCurve],
) -> Result<Vec<f32>, Box<dyn Error>> {
	instance.initialize(sample_rate)?;

	let mut output = Vec::with_capacity(n_samples * 2);
	let mut event_idx = 0;
	let mut cursors = vec![0usize; curves.len()];

	for sample in 0..n_samples {
		// Set each automated parameter to its linearly interpolated value
		for (i, curve) in curves.iter().enumerate() {
			instance.set_parameter(curve.index, interpolate_curve(&curve.points, sample, &mut cursors[i]))?;
		}
		// Dispatch note events (note-offs before note-ons within same sample)
		while event_idx < events.len() && events[event_idx].0 == sample {
			match &events[event_idx].1 {
				MusicEvent::NoteOn { channel, key, velocity } => {
					instance.note_on(*channel, *key, *velocity)?;
				}
				MusicEvent::NoteOff { channel, key } => {
					instance.note_off(*channel, *key)?;
				}
			}
			event_idx += 1;
		}
		let s = instance.next_sample()?;
		output.push(s[0] as f32);
		output.push(s[1] as f32);
	}

	Ok(output)
}

fn play_file(options: &PlayOptions) -> Vec<PathBuf> {
	let compile = |filename: &str, contents: String| {
		let mut compiler = compiler::Compiler::new(filename.into(), contents);
		let compile_result = compiler.compile();
		let sources = compiler.sources();
		let ast = compiler.ast().cloned();
		(compile_result, sources, ast)
	};
	let filename = &options.zing_file;
	match fs::read_to_string(filename) {
		Ok(s) => match compile(filename, s) {
			(Ok(program), sources, ast) => {
				if options.pretty_print && let Some(ast) = ast {
					println!("{}", ast);
				}

				// Load music from xrns once; used for both write_source and audio output
				let music = if let Some(xrns) = &options.xrns {
					match convert_renoise_file(xrns) {
						Ok(music) => Some(music),
						Err(e) => {
							println!("Error converting Renoise file '{}': {}", xrns, e);
							None
						}
					}
				} else {
					None
				};

				if let Some(filename) = &options.write_source {
					let empty = Music::empty();
					let music_ref = music.as_ref().unwrap_or(&empty);
					match File::create(filename) {
						Ok(mut file) => {
							if let Err(e) = convert_music_with_program(music_ref,
									&program, &options.jingler_asm_path,
									options.sample_rate, !options.byte_index, options.quantization_levels,
									&mut file) {
								println!("Error writing output file '{}': {}", filename, e);
							}
						},
						Err(e) => {
							println!("Error creating output file '{}': {}", filename, e);
						},
					}
				}
				if options.connect {
					if let Err(e) = send_program(&program, &options.address) {
						println!("Error sending program: {}", e);
					}
				}
				if options.print_ir {
					for (p, proc) in program.procedures.iter().enumerate() {
						println!("{:2}: {}", p, proc);
						for (i, inst) in proc.code.iter().enumerate() {
							println!("{:5}  {}", i, inst);
						}
						println!();
					}
				}
				if options.write_wasm.is_some() || options.play || options.write_wav.is_some() {
					let (rt, mut instance) = default_jingler_runtime().unwrap();
					match rt.submit_program(&program) {
						Err(e) => {
							println!("Runtime error: {}", e);
						}
						Ok(_) => {
							if let Err(e) = instance.poll_pending() {
								println!("Runtime error: {}", e);
							}
							if let Some(ref filename) = options.write_wasm {
								match instance.dump() {
									Some(bytes) => {
										if let Err(e) = fs::write(filename, bytes) {
											println!("Error writing Wasm to '{}': {}", filename, e);
										}
									}
									None => {
										println!("Error: no instance available to dump");
									}
								}
							}
							if options.play || options.write_wav.is_some() {
								let (n_samples, events, curves) = if let Some(ref music) = music {
									let n_samples = (music.length as f32 * music.ticklength * options.sample_rate) as usize;
									let events = build_music_events(music, options.sample_rate);
									let curves = build_parameter_curves(music, options.sample_rate);
									(n_samples, events, curves)
								} else {
									((options.duration * options.sample_rate) as usize, vec![], vec![])
								};

								match compute_audio(&mut *instance, options.sample_rate, n_samples, &events, &curves) {
									Ok(output) => {
										if let Some(ref wav_filename) = options.write_wav {
											if let Err(e) = write_wav(wav_filename, options.sample_rate, &output) {
												println!("Error writing wav file '{}': {}", wav_filename, e);
											}
										}
										if options.play {
											if let Err(e) = play_sound(options.sample_rate, &output) {
												println!("Error playing sound: {}", e);
											}
										}
									}
									Err(e) => {
										println!("Runtime error: {}", e);
									}
								}
							}
						}
					}
				}
				sources
			},
			(Err(errors), sources, _) => {
				for message in errors {
					println!("{}", message);
				}
				sources
			}
		},
		Err(e) => {
			println!("Error reading '{}': {}", filename, e);
			vec![]
		}
	}
}

fn play_file_resident(options: &PlayOptions) -> Result<(), Box<dyn Error>> {
	let (tx, rx) = channel::<DebounceEventResult>();
	let mut debouncer = new_debouncer(Duration::from_secs_f32(0.1), move |res| {
		let _ = tx.send(res);
	})?;

	let mut watched_files = play_file(options);
	for file in &watched_files {
		debouncer.watcher().watch(file, RecursiveMode::NonRecursive)?;
	}

	loop {
		match rx.recv()? {
			Ok(_events) => {
				println!("Reloading '{}' at {}", options.zing_file, Local::now().to_rfc2822());
				for file in &watched_files {
					let _ = debouncer.watcher().unwatch(file);
				}
				watched_files = play_file(options);
				for file in &watched_files {
					let _ = debouncer.watcher().watch(file, RecursiveMode::NonRecursive);
				}
			},
			Err(e) => {
				Err(e)?;
			},
		}
	}
}

fn main() {
	let options = PlayOptions::parse();

	if options.resident {
		if let Err(e) = play_file_resident(&options) {
			println!("Error watching file '{}': {}", options.zing_file, e);
		}
	} else {
		play_file(&options);
	}
}
