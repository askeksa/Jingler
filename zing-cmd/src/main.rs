
use bytecode::encode::encode_bytecodes;
use zing::compiler;

use std::error::Error;
use std::fs;
use std::slice;
use std::sync::mpsc::channel;
use std::time::{Duration};

use chrono::Local;
use clap::clap_app;
use hound::{SampleFormat, WavSpec, WavWriter};
use notify::{DebouncedEvent, RecursiveMode, Watcher, watcher};
use rodio::buffer::SamplesBuffer;
use rodio::Sink;

#[link(name = "clinklang_cmd")]
extern "C" {
    fn RunClinklang(bytecodes: *const u8, constants: *const u32, length: usize) -> *mut f32;
}

fn run(bytecodes: &[u8], constants: &[u32], length: usize) -> &'static [f32] {
	unsafe {
		let music = RunClinklang(bytecodes.as_ptr(), constants.as_ptr(), length);
		slice::from_raw_parts(music, length * 2)
	}
}

struct PlayOptions {
	sample_rate: f32,
	duration: Duration,
	run: bool,
	play: bool,
	dump_bytecodes: bool,
	write_wav: Option<String>,
}

impl Default for PlayOptions {
	fn default() -> Self {
		PlayOptions {
			sample_rate: 44100.0,
			duration: Duration::from_secs(1),
			run: true,
			play: true,
			dump_bytecodes: false,
			write_wav: None,
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
				if options.dump_bytecodes {
					for (i, bc) in program.iter().enumerate() {
						println!("{:5}  {}", i, bc);
					}
				}
				match encode_bytecodes(&program, options.sample_rate) {
					Ok((bytecodes, constants)) => if options.run {
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
		(version: "0.1.0")
		(@arg ZING: +required "Zing file to play.")
		(@arg SAMPLE_RATE: -s --samplerate +takes_value "Sample rate to play at.")
		(@arg DURATION: -d --duration +takes_value "Duration of audio, in seconds.")
		(@arg WAV_FILE: -w --writewav +takes_value "Write WAV file.")
		(@arg COMPILE_ONLY: -c --compileonly "Don't run generated audio code.")
		(@arg SILENT: -n --noaudio "Don't play audio.")
		(@arg DUMP: -b --bytecodes "Dump generated bytecodes.")
		(@arg RESIDENT: -r --resident "Stay resident and reload file when it changes.")
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
		options.dump_bytecodes = true;
	}
	if let Some(wav_filename) = matches.value_of("WAV_FILE") {
		options.write_wav = Some(wav_filename.to_string());
	}

	if matches.is_present("RESIDENT") {
		if let Err(e) = play_file_resident(zing_filename, &options) {
			println!("Error watching file '{}': {}", zing_filename, e);
		}
	} else {
		play_file(zing_filename, &options);
	}
}
