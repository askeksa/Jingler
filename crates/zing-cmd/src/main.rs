use convert::{Music, convert_music_with_program, renoise::convert_renoise_file};
use runtime::default_jingler_runtime;
use zing::compiler;

use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::net::TcpStream;
use std::path::PathBuf;
use std::sync::mpsc::channel;
use std::time::{Duration};

use chrono::Local;
use clap::Parser;
use hound::{SampleFormat, WavSpec, WavWriter};
use notify_debouncer_mini::notify::RecursiveMode;
use notify_debouncer_mini::{new_debouncer, DebounceEventResult};
use rodio::buffer::SamplesBuffer;
use rodio::{OutputStream, Sink};

const DEFAULT_CONNECT_ADDR: &str = "127.0.0.1:26127";

#[derive(Parser)]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct PlayOptions {
	/// Zing file to play.
	zing_file: String,

	/// Stay resident and reload file when it changes.
	#[arg(short, long)]
	resident: bool,

	/// Print IR to stdout.
	#[arg(short, long, help_heading = "Code output")]
	ir: bool,

	/// Dump generated Wasm to file.
	#[arg(short, long, value_name = "WASM_FILE", help_heading = "Code output")]
	generated_wasm: Option<String>,

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

	/// Renoise file containing music to convert.
	#[arg(short, long, value_name = "RENOISE_FILE", help_heading = "Source output options")]
	xrns: Option<String>,

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
	stream.flush()?;
	println!("Sent program to {} ({} bytes)", address, data.len());
	Ok(())
}

fn play_file(options: &PlayOptions) -> Vec<PathBuf> {
	let compile = |filename: &str, contents: String| {
		let mut compiler = compiler::Compiler::new(filename.into(), contents);
		let compile_result = compiler.compile();
		let sources = compiler.sources();
		(compile_result, sources)
	};
	let filename = &options.zing_file;
	match fs::read_to_string(filename) {
		Ok(s) => match compile(filename, s) {
			(Ok(program), sources) => {
				if let Some(filename) = &options.output {
					let music = if let Some(xrns) = &options.xrns {
						match convert_renoise_file(xrns) {
							Ok(music) => music,
							Err(e) => {
								println!("Error converting Renoise file '{}': {}", xrns, e);
								Music::empty()
							}
						}
					} else {
						Music::empty()
					};
					match File::create(filename) {
						Ok(mut file) => {
							if let Err(e) = convert_music_with_program(&music,
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
				if options.ir {
					for (p, proc) in program.procedures.iter().enumerate() {
						println!("{:2}: {}", p, proc);
						for (i, inst) in proc.code.iter().enumerate() {
							println!("{:5}  {}", i, inst);
						}
						println!();
					}
				}
				if options.generated_wasm.is_some() || options.play || options.write_wav.is_some() {
					let runtime = default_jingler_runtime().unwrap();
					match runtime.load_program(&program) {
						Err(e) => {
							println!("Runtime error: {}", e);
						}
						Ok(mut instance) => {
							if let Some(ref filename) = options.generated_wasm {
								if let Err(e) = fs::write(filename, instance.dump()) {
									println!("Error writing Wasm to '{}': {}", filename, e);
								}
							}
							if let Err(e) = instance.initialize(options.sample_rate) {
								println!("Runtime error: {}", e);
							} else {
								let n_samples = (options.duration * options.sample_rate) as usize;
								let output = (0..n_samples)
									.map(|_| instance.next_sample().unwrap())
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
							}
						}
					}
				}
				sources
			},
			(Err(errors), sources) => {
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
