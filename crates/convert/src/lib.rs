pub mod renoise;
pub mod xml;

use anyhow::Result;

use std::io::Write;

use ir::encode::encode_bytecodes_source;

pub fn convert_music_with_program(music: &Music,
		program: &ir::Program, jingler_asm_path: &String,
		sample_rate: f32, embed_constant_index: bool, quantization_levels: u16,
		out: &mut impl std::io::Write) -> Result<()> {
	let parameter_quantization = 1.0 / (quantization_levels as f32);

	music.export(out, sample_rate, &program.track_order, program.parameters.len(), quantization_levels)?;
	encode_bytecodes_source(program, jingler_asm_path, sample_rate, embed_constant_index, parameter_quantization, out)?;

	Ok(())
}


// Data Structures

#[derive(Clone, Debug)]
pub struct Instrument {
	pub name: String,
	pub channel: u16,
}

#[derive(Clone, Debug)]
pub struct Note {
	pub column: u16,
	pub line: u32,
	pub length: Option<u32>,
	pub songpos: u32,
	pub pat: u16,
	pub patline: u16,
	pub instr: u16,
	pub key: u16,
	pub velocity: u16,
}

#[derive(Clone, Debug)]
pub struct AutomationPoint {
	pub line: u32,
	pub value: f32,
}

#[derive(Clone, Debug)]
pub struct Track {
	pub name: String,
	pub instr: u16,
	pub notes: Vec<Note>,
	pub labelname: String,
}

#[derive(Clone, Debug, Default)]
pub struct Music {
	pub tracks: Vec<Track>,
	pub instruments: Vec<Instrument>,
	pub length: u32,
	pub ticklength: f32,
	pub autos: Vec<Vec<AutomationPoint>>,
	pub channel_map: Vec<Option<usize>>,
}

// Export logic
impl Music {
	pub fn empty() -> Self {
		Self::default()
	}

	pub fn export(&self, w: &mut dyn Write,
			sample_rate: f32, track_order: &[usize],
			num_parameters: usize, quantization_levels: u16) -> std::io::Result<()> {
		let spt = (self.ticklength * sample_rate).round() as u32;
		let total_samples = (self.length as f32 * self.ticklength * sample_rate) as u64;

		let roundup = |v: u64| (v & !0xFFFF) + 0x10000;

		let quantized_parameters = self.quantize_parameters(num_parameters, quantization_levels);

		writeln!(w, "%define SAMPLE_RATE {:.0}", sample_rate)?;
		writeln!(w)?;
		writeln!(w, "%define MUSIC_LENGTH {}", self.length)?;
		writeln!(w, "%define TOTAL_SAMPLES {}", roundup(total_samples))?;
		writeln!(w)?;
		writeln!(w, "%define SAMPLES_PER_TICK {}", spt)?;
		writeln!(w, "%define TICKS_PER_SECOND {:.9}", sample_rate as f64 / spt as f64)?;
		writeln!(w)?;
		writeln!(w, "section musdat data align=1")?;
		writeln!(w)?;
		writeln!(w, "MusicData:")?;
		writeln!(w)?;

		// Helper closures
		let encode_distance = |dist: u32| -> Vec<u8> {
			if dist < 128 { vec![dist as u8] } else { vec![255 - (dist >> 8) as u8, (dist & 255) as u8] }
		};

		// Velocities
		writeln!(w, "Velocities:\n\tdd\t1")?;
		self.notelist(w, track_order,
			|_, n| vec![n.velocity as u8],
			vec![0x80], ".v_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, _| vec![],
			vec![0x80]
		)?;

		// Keys
		writeln!(w, "Keys:\n\tdd\t1")?;
		self.notelist(w, track_order,
			|_, n| vec![n.key as u8],
			vec![0x80], ".k_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, p| vec![p.1],
			vec![0x80]
		)?;

		// Lengths
		writeln!(w, "Lengths:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order,
			|_, n| encode_distance(n.length.unwrap_or(0x7E00)),
			vec![0x80], ".l_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, _| vec![],
			vec![0x80]
		)?;

		// Distances
		writeln!(w, "Distances:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order,
			|prev_n, n| {
				let prev_line = prev_n.map(|pn| pn.line).unwrap_or(0);
				encode_distance(n.line - prev_line)
			},
			vec![0x80], ".d_"
		)?;

		self.real_autolist_distance(w, &quantized_parameters)?;
		Ok(())
	}

	fn quantize_parameters(&self, num_parameters: usize, quantization_levels: u16) -> Vec<Vec<(u32, u8)>> {
		let empty = vec![];
		let mut result = Vec::new();
		for i in 0..num_parameters {
			let points = self.autos.get(i).unwrap_or(&empty);
			let mut quantized: Vec<(u32, u8)> = Vec::new();
			for p in points {
				let value = (p.value * quantization_levels as f32).round() as u8;
				if let [.., (_, v1), (_, v2)] = &quantized[..] && *v1 == value && *v2 == value {
					quantized.pop();
				}
				quantized.push((p.line, value));
			}
			result.push(quantized);
		}
		result
	}

	// Helper to format data lines
	fn dataline(w: &mut dyn Write, data: &[u8]) -> std::io::Result<()> {
		if data.is_empty() { return Ok(()); }
		write!(w, "\tdb\t")?;
		for (i, d) in data.iter().enumerate() {
			if i > 0 { write!(w, ",")?; }
			write!(w, "{}", d)?;
		}
		writeln!(w)?;
		Ok(())
	}

	fn notelist<F>(&self, w: &mut dyn Write, track_order: &[usize], mut datafunc: F, trackterm: Vec<u8>, prefix: &str) -> std::io::Result<()>
	where F: FnMut(Option<&Note>, &Note) -> Vec<u8> {

		for (i, &channel) in track_order.iter().enumerate() {
			if channel < 16 && let Some(track_idx) = self.channel_map[channel] {
				let track = &self.tracks[track_idx];
				writeln!(w, "\t; {}", track.name)?;
				writeln!(w, "{}{}_{}_{}:", prefix, i, track.labelname, track.instr)?;

				let mut prev_n: Option<&Note> = None;
				let mut last_songpos: Option<u32> = None;
				let mut pat_data = Vec::new();
				for n in &track.notes {
					let trigger_new_line = if let Some(lp) = last_songpos { n.songpos != lp } else { true };

					if trigger_new_line {
						Self::dataline(w, &pat_data)?;
						pat_data.clear();
						writeln!(w, "\t; Position {}, pattern {}", n.songpos, n.pat)?;
						last_songpos = Some(n.songpos);
					}

					pat_data.extend(datafunc(prev_n, n));
					prev_n = Some(n);
				}
				Self::dataline(w, &pat_data)?;
			}
			Self::dataline(w, &trackterm)?;
			writeln!(w)?;
		}
		Ok(())
	}

	fn autolist<F>(&self, w: &mut dyn Write, parameters: &Vec<Vec<(u32, u8)>>, mut datafunc: F, trackterm: Vec<u8>) -> std::io::Result<()>
	where F: FnMut(Option<&(u32, u8)>, &(u32, u8)) -> Vec<u8> {
		for (i, points) in parameters.iter().enumerate() {
			writeln!(w, "\t; Parameter {}", i)?;
			writeln!(w, ".p_{}:", i)?;

			let mut data = Vec::new();
			let mut prev_p: Option<&(u32, u8)> = None;
			for p in points {
				data.extend(datafunc(prev_p, p));
				prev_p = Some(p);
			}
			Self::dataline(w, &data)?;
			Self::dataline(w, &trackterm)?;
			writeln!(w)?;
		}
		Ok(())
	}

	fn real_autolist_distance(&self, w: &mut dyn Write, parameters: &Vec<Vec<(u32, u8)>>) -> std::io::Result<()> {
		let encode_distance = |dist: u32| -> Vec<u8> {
			if dist < 128 { vec![dist as u8] } else { vec![255 - (dist >> 8) as u8, (dist & 255) as u8] }
		};

		self.autolist(w, parameters, |prev_p, p| {
			let prev_line = prev_p.map(|pp| pp.0).unwrap_or(0);
			encode_distance(p.0 - prev_line)
		}, vec![0x80])
	}
}
