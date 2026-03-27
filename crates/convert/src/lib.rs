pub mod renoise;

use std::io::Write;

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
	pub value: u16,
}

#[derive(Clone, Debug)]
pub struct Track {
	pub name: String,
	pub instr: u16,
	pub notes: Vec<Note>,
	pub labelname: String,
}

#[derive(Clone, Debug)]
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
	pub fn export(&self, w: &mut dyn Write, sample_rate: f32, track_order: &[u16]) -> std::io::Result<()> {
		let spt = (self.ticklength * sample_rate).round() as u32;
		let total_samples = (self.length as f32 * self.ticklength * sample_rate) as u64;

		let roundup = |v: u64| (v & !0xFFFF) + 0x10000;

		writeln!(w, "%define SAMPLE_RATE {:.0}", sample_rate)?;
		writeln!(w)?;
		writeln!(w, "%define NUM_TRACKS {}", track_order.len())?;
		writeln!(w)?;
		writeln!(w, "%define MUSIC_LENGTH {}", self.length)?;
		writeln!(w, "%define TOTAL_SAMPLES {}", roundup(total_samples))?;
		writeln!(w)?;
		writeln!(w, "%define SAMPLES_PER_TICK {}", spt)?;
		writeln!(w, "%define TICKS_PER_SECOND {:.9}", sample_rate as f64 / spt as f64)?;
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
		self.autolist(w, |_, _| vec![], vec![0x80])?;

		// Keys
		writeln!(w, "Keys:\n\tdd\t1")?;
		self.notelist(w, track_order,
			|_, n| vec![n.key as u8],
			vec![0x80], ".k_"
		)?;
		self.autolist(w, |_, p| vec![p.value as u8], vec![0x80])?;

		// Lengths
		writeln!(w, "Lengths:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order,
			|_, n| encode_distance(n.length.unwrap_or(0x7E00)),
			vec![0x80], ".l_"
		)?;
		self.autolist(w, |_, _| vec![], vec![0x80])?;

		// Distances
		writeln!(w, "Distances:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order,
			|prev_n, n| {
				let prev_line = prev_n.map(|pn| pn.line).unwrap_or(0);
				encode_distance(n.line - prev_line)
			},
			vec![0x80], ".d_"
		)?;

		self.real_autolist_distance(w)?;
		Ok(())
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

	fn notelist<F>(&self, w: &mut dyn Write, track_order: &[u16], mut datafunc: F, trackterm: Vec<u8>, prefix: &str) -> std::io::Result<()>
	where F: FnMut(Option<&Note>, &Note) -> Vec<u8> {

		for (i, &channel) in track_order.iter().enumerate() {
			if channel < 16 && let Some(track_idx) = self.channel_map[channel as usize] {
				let track = &self.tracks[track_idx as usize];
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

	fn autolist<F>(&self, w: &mut dyn Write, mut datafunc: F, trackterm: Vec<u8>) -> std::io::Result<()>
	where F: FnMut(Option<&AutomationPoint>, &AutomationPoint) -> Vec<u8> {
		for (i, points) in self.autos.iter().enumerate() {
			writeln!(w, "\t; Parameter {}", i)?;
			writeln!(w, ".p_{}:", i)?;

			let mut data = Vec::new();
			let mut prev_p: Option<&AutomationPoint> = None;
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

	fn real_autolist_distance(&self, w: &mut dyn Write) -> std::io::Result<()> {
		let encode_distance = |dist: u32| -> Vec<u8> {
			if dist < 128 { vec![dist as u8] } else { vec![255 - (dist >> 8) as u8, (dist & 255) as u8] }
		};

		self.autolist(w, |prev_p, p| {
			let prev_line = prev_p.map(|pp| pp.line).unwrap_or(0);
			encode_distance(p.line - prev_line)
		}, vec![0x80])
	}
}
