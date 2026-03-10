
use ir::encode::encode_bytecodes_source;
use runtime::{JinglerRuntime, NativeRuntime};
use zing::compiler;

use std::error::Error;
use std::fs;
use std::fs::File;
use std::net::TcpStream;
use std::sync::mpsc::channel;
use std::time::{Duration};

use std::io::Write;

use chrono::Local;
use clap::Parser;
use hound::{SampleFormat, WavSpec, WavWriter};
use notify::{DebouncedEvent, RecursiveMode, Watcher, watcher};
use rodio::buffer::SamplesBuffer;
use rodio::{OutputStream, Sink};

const DEFAULT_CONNECT_ADDR: &str = "localhost:26127";

#[derive(Parser)]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct PlayOptions {
	/// Zing file to play.
	zing_file: String,

	/// Stay resident and reload file when it changes.
	#[arg(short, long)]
	resident: bool,

	/// Dump generated code.
	#[arg(short, long, help_heading = "Code output")]
	dump: bool,

	/// Send the program to a listening plugin.
	#[arg(short, long, help_heading = "Code output")]
	connect: bool,

	/// Output source file for music.
	#[arg(short, long, value_name = "OUTPUT_FILE", help_heading = "Code output")]
	output: Option<String>,

	/// Play audio.
	#[arg(short, long, help_heading = "Audio output")]
	play: bool,

	/// Write WAV file.
	#[arg(short, long, value_name = "WAV_FILE", help_heading = "Audio output")]
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
	let (_stream, stream_handle) = OutputStream::try_default()
		.map_err(|e| format!("Could not open default device: {e}"))?;
	let sink = Sink::try_new(&stream_handle)
		.map_err(|e| format!("Could not create audio sink: {e}"))?;
	let buffer = SamplesBuffer::new(2, sample_rate as u32, data);
	sink.append(buffer);
	sink.sleep_until_end();
	Ok(())
}

fn send_program(program: &ir::Program, address: &str) -> Result<(), Box<dyn Error>> {
	let mut stream = TcpStream::connect(address)?;
	let data = bincode::serialize(program)?;
	let len = data.len() as u32;
	stream.write_all(&len.to_le_bytes())?;
	stream.write_all(&data)?;
	println!("Sent program to {} ({} bytes)", address, data.len());
	Ok(())
}

fn play_file(options: &PlayOptions) {
	let filename = &options.zing_file;
	match fs::read_to_string(filename) {
		Ok(s) => match compiler::Compiler::new(filename.to_string(), s).compile() {
			Ok(program) => {
				if let Some(filename) = &options.output {
					match File::create(filename) {
						Ok(mut file) => {
							let parameter_quantization = 1.0 / (options.quantization_levels as f32);
							if let Err(e) = encode_bytecodes_source(
									&program, &options.jingler_asm_path,
									options.sample_rate, !options.byte_index, parameter_quantization,
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
				if options.connect {
					if let Err(e) = send_program(&program, &options.address) {
						println!("Error sending program: {}", e);
					}
				}
				if options.dump {
					for (p, proc) in program.procedures.iter().enumerate() {
						println!("{:2}: {}", p, proc);
						for (i, inst) in proc.code.iter().enumerate() {
							println!("{:5}  {}", i, inst);
						}
						println!();
					}
				}
				if options.play || options.write_wav.is_some() {
					let mut runtime = NativeRuntime::new();
					if let Err(e) = runtime.load_program(program, options.sample_rate) {
						println!("Runtime error: {}", e);
					} else {
						let n_samples = (options.duration * options.sample_rate) as usize;
						let output = (0..n_samples)
							.map(|_| runtime.next_sample())
							.flatten()
							.map(|s| s as f32)
							.collect::<Vec<f32>>();

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

						runtime.unload_program();
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

fn play_file_resident(options: &PlayOptions) -> Result<(), Box<dyn Error>> {
	let filename = &options.zing_file;
	let (tx, rx) = channel();
	let mut watcher = watcher(tx, Duration::from_secs_f32(0.1))?;
	watcher.watch(filename, RecursiveMode::NonRecursive)?;

	play_file(options);

	loop {
		match rx.recv()? {
			DebouncedEvent::Write(_) => {
				println!("Reloading '{}' at {}", filename, Local::now().to_rfc2822());
				play_file(options);
			},
			DebouncedEvent::Error(e, _) => {
				Err(e)?;
			},
			_ => {},
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
