
use ir::encode::{encode_bytecodes_binary, encode_bytecodes_source};
use zing::compiler;

use std::error::Error;
use std::fs;
use std::fs::File;
use std::sync::mpsc::channel;
use std::time::{Duration};

use chrono::Local;
use clap::clap_app;
use hound::{SampleFormat, WavSpec, WavWriter};
use notify::{DebouncedEvent, RecursiveMode, Watcher, watcher};
use rodio::buffer::SamplesBuffer;
use rodio::Sink;

#[cfg(target_arch = "x86")]
#[link(name = "jingler_cmd")]
extern "C" {
	fn CompileBytecode(bytecodes: *const u8);
	fn ReleaseBytecode();
	fn RunStaticCode(constants: *const u32);
	fn RenderSamples(constants: *const u32, length: usize) -> *mut f32;
}

#[cfg(target_arch = "x86")]
fn run(bytecodes: &[u8], constants: &[u32], length: usize) -> &'static [f32] {
	unsafe {
		CompileBytecode(bytecodes.as_ptr());
		RunStaticCode(constants.as_ptr());
		let music = RenderSamples(constants.as_ptr(), length);
		ReleaseBytecode();
		std::slice::from_raw_parts(music, length * 2)
	}
}

#[cfg(not(target_arch = "x86"))]
fn run(_bytecodes: &[u8], _constants: &[u32], _length: usize) -> &'static [f32] {
	&EMPTY_SOUND
}

#[cfg(not(target_arch = "x86"))]
const EMPTY_SOUND: [f32; 0] = [0f32; 0];

struct PlayOptions {
	sample_rate: f32,
	duration: Duration,
	run: bool,
	play: bool,
	dump_instructions: bool,
	write_wav: Option<String>,
	write_source: Option<String>,
	jingler_asm_path: String,
	parameter_quantization_levels: u16,
}

impl Default for PlayOptions {
	fn default() -> Self {
		PlayOptions {
			sample_rate: 44100.0,
			duration: Duration::from_secs(1),
			run: true,
			play: true,
			dump_instructions: false,
			write_wav: None,
			write_source: None,
			jingler_asm_path: "jingler.asm".to_string(),
			parameter_quantization_levels: 16,
		}
	}
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
	let device = rodio::default_output_device().ok_or("Could not open default device.")?;
	let sink = Sink::new(&device);
	let buffer = SamplesBuffer::new(2, sample_rate as u32, data);
	sink.append(buffer);
	sink.sleep_until_end();
	Ok(())
}

fn play_file(filename: &str, options: &PlayOptions) {
	match fs::read_to_string(&filename) {
		Ok(s) => match compiler::Compiler::new(&filename, &s).compile() {
			Ok(program) => {
				if let Some(filename) = &options.write_source {
					match File::create(filename) {
						Ok(mut file) => {
							let parameter_quantization = 1.0 / (options.parameter_quantization_levels as f32);
							if let Err(e) = encode_bytecodes_source(
									&program, &options.jingler_asm_path,
									options.sample_rate, parameter_quantization,
									&mut file) {
								println!("Error writing output file '{}': {}", filename, e);
							}
						},
						Err(e) => {
							println!("Error creating output file '{}': {}", filename, e);
						},
					}

					println!("Parameters: {}", program.parameters.len());
					print!("Track order:");
					for channel in &program.track_order {
						print!(" {}", channel);
					}
					println!();
				}
				if options.dump_instructions {
					for (p, proc) in program.procedures.iter().enumerate() {
						println!("{:2}: {}", p, proc);
						for (i, inst) in proc.code.iter().enumerate() {
							println!("{:5}  {}", i, inst);
						}
						println!();
					}
				}
				match encode_bytecodes_binary(&program, options.sample_rate) {
					Ok((bytecodes, constants, _parameter_offset)) => if options.run {
						let n_samples = (options.duration.as_secs_f32() * options.sample_rate) as usize;
						let output = run(&bytecodes[..], &constants[..], n_samples);

						if let Some(ref wav_filename) = options.write_wav {
							if let Err(e) = write_wav(wav_filename, options.sample_rate, output) {
								println!("Error writing wav file '{}': {}", wav_filename, e);
							}
						}

						if options.play {
							if let Err(e) = play_sound(options.sample_rate, output) {
								println!("Error playing sound: {}", e);
							}
						}
					},
					Err(message) => {
						println!("Encoding error: {}", message);
					}
				}
			},
			Err(errors) => for message in errors {
				println!("{}", message);
			}
		},
		Err(e) => {
			println!("Error reading '{}': {}", filename, e);
		}
	}
}

fn play_file_resident(filename: &str, options: &PlayOptions) -> Result<(), Box<dyn Error>> {
	let (tx, rx) = channel();
	let mut watcher = watcher(tx, Duration::from_secs_f32(0.1))?;
	watcher.watch(filename, RecursiveMode::NonRecursive)?;

	play_file(filename, options);

	loop {
		match rx.recv()? {
			DebouncedEvent::Write(_) => {
				println!("Reloading '{}' at {}", filename, Local::now().to_rfc2822());
				play_file(filename, options);
			},
			DebouncedEvent::Error(e, _) => {
				Err(e)?;
			},
			_ => {},
		}
	}
}

fn main() {
	let matches = clap_app!(zing =>
		(version: "0.2.0")
		(@arg ZING: +required "Zing file to play.")
		(@arg SAMPLE_RATE: -s --samplerate +takes_value "Sample rate to play at.")
		(@arg DURATION: -d --duration +takes_value "Duration of audio, in seconds.")
		(@arg WAV_FILE: -w --writewav +takes_value "Write WAV file.")
		(@arg COMPILE_ONLY: -c --compileonly "Don't run generated audio code.")
		(@arg SILENT: -n --noaudio "Don't play audio.")
		(@arg DUMP: -g --dump "Dump generated code.")
		(@arg RESIDENT: -r --resident "Stay resident and reload file when it changes.")
		(@arg OUTPUT: -o --output +takes_value "Output source file for music.")
		(@arg JINGLER_ASM: -j --jinglerasm +takes_value "Path to jingler.asm file.")
		(@arg QUANTIZATION: -q --quantization +takes_value "Number of quantization levels for parameters.")
	).get_matches();

	let zing_filename = matches.value_of("ZING").unwrap();

	let mut options = PlayOptions::default();
	if let Some(sample_rate) = matches.value_of("SAMPLE_RATE") {
		match sample_rate.parse::<f32>() {
			Ok(sample_rate) => {
				options.sample_rate = sample_rate;
			},
			Err(_) => {
				println!("Invalid sample rate: {}", sample_rate);
			},
		}
	}
	if let Some(duration) = matches.value_of("DURATION") {
		match duration.parse::<f32>() {
			Ok(duration) => {
				options.duration = Duration::from_secs_f32(duration);
			},
			Err(_) => {
				println!("Invalid duration: {}", duration);
			},
		}
	}
	if matches.is_present("COMPILE_ONLY") {
		options.run = false;
	}
	if matches.is_present("SILENT") {
		options.play = false;
	}
	if matches.is_present("DUMP") {
		options.dump_instructions = true;
	}
	if let Some(wav_filename) = matches.value_of("WAV_FILE") {
		options.write_wav = Some(wav_filename.to_string());
	}
	if let Some(filename) = matches.value_of("OUTPUT") {
		options.write_source = Some(filename.to_string());
	}
	if let Some(jingler_asm_path) = matches.value_of("JINGLER_ASM") {
		options.jingler_asm_path = jingler_asm_path.to_string();
	}
	if let Some(q) = matches.value_of("QUANTIZATION") {
		match q.parse::<u16>() {
			Ok(q) => {
				options.parameter_quantization_levels = q;
			},
			Err(_) => {
				println!("Invalid quantization: {}", q);
			},
		}
	}

	if matches.is_present("RESIDENT") {
		if let Err(e) = play_file_resident(zing_filename, &options) {
			println!("Error watching file '{}': {}", zing_filename, e);
		}
	} else {
		play_file(zing_filename, &options);
	}
}
