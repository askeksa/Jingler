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

	let track_procedures = program.track_procedure_names();
	music.export(out, sample_rate, &program.track_order, &track_procedures,
		program.parameters.len(), quantization_levels)?;
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
	pub track: u16,
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
	pub labelname: String,
}

#[derive(Clone, Debug, Default)]
pub struct Music {
	pub tracks: Vec<Track>,
	pub instruments: Vec<Instrument>,
	/// All notes of the song, sorted by time. Every note names the track it
	/// belongs to, so a track can hold any number of instruments and the notes
	/// of an instrument can be spread across any number of tracks.
	pub notes: Vec<Note>,
	pub length: u32,
	pub ticklength: f32,
	pub autos: Vec<Vec<AutomationPoint>>,
}

/// The values of `iter` with duplicates removed, in order of first appearance.
fn distinct<T: Eq + Copy>(iter: impl Iterator<Item = T>) -> Vec<T> {
	let mut result: Vec<T> = Vec::new();
	for value in iter {
		if !result.contains(&value) {
			result.push(value);
		}
	}
	result
}

/// A name reduced to the characters that are valid in an assembler label.
fn labelify(name: &str) -> String {
	name.chars().filter(|c| c.is_alphanumeric() || *c == '_').collect()
}

/// The label of a player track: its index, the procedure played by it and -
/// each only when the whole track agrees on it - its track and instrument name.
fn label_for_track(prefix: &str, index: usize, procedure: Option<&str>,
		track_names: &[String], instrument_names: &[String]) -> String {
	let mut label = format!("{}{}", prefix, index);
	for part in [procedure, single(track_names), single(instrument_names)] {
		if let Some(part) = part {
			let part = labelify(part);
			if !part.is_empty() {
				label += &format!("_{}", part);
			}
		}
	}
	label
}

/// A human-readable description of a player track for the comment above its label.
fn describe_track(mapping: &ir::MidiMapping, procedure: Option<&str>,
		instrument_names: &[String], track_names: &[String]) -> String {
	// MIDI channels are one-based in Zing source and in Renoise.
	let mut description = format!("Channel {}, keys {}-{}",
		mapping.channel as u16 + 1, mapping.start, mapping.end);
	if mapping.transpose_to != mapping.start {
		description += &format!(" -> {}", mapping.transpose_to);
	}
	if let Some(procedure) = procedure {
		description += &format!(": {}", procedure);
	}
	description += &name_list("instrument", "instruments", instrument_names);
	description += &name_list("track", "tracks", track_names);
	if instrument_names.is_empty() {
		description += ", no notes";
	}
	description
}

/// The only element of `names`, or `None` if there is not exactly one.
fn single(names: &[String]) -> Option<&str> {
	match names {
		[name] => Some(name),
		_ => None,
	}
}

fn name_list(singular: &str, plural: &str, names: &[String]) -> String {
	if names.is_empty() { return String::new(); }
	let noun = if names.len() == 1 { singular } else { plural };
	let quoted: Vec<String> = names.iter().map(|n| format!("'{}'", n)).collect();
	format!(", {} {}", noun, quoted.join(", "))
}

// Export logic
impl Music {
	pub fn empty() -> Self {
		Self::default()
	}

	pub fn export(&self, w: &mut dyn Write,
			sample_rate: f32, track_order: &[ir::MidiMapping], track_procedures: &[&str],
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

		// The notes of each player track, in the order they are played
		let tracks: Vec<Vec<&Note>> = track_order.iter()
			.map(|mapping| self.track_notes(mapping))
			.collect();

		// Velocities
		writeln!(w, "Velocities:\n\tdd\t1")?;
		self.notelist(w, track_order, track_procedures, &tracks,
			|_, n, _| vec![n.velocity as u8],
			vec![0x80], ".v_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, _| vec![],
			vec![0x80]
		)?;

		// Keys
		writeln!(w, "Keys:\n\tdd\t1")?;
		self.notelist(w, track_order, track_procedures, &tracks,
			|_, n, m| vec![m.triggered_key(m.channel, n.key as u8).unwrap()],
			vec![0x80], ".k_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, p| vec![p.1],
			vec![0x80]
		)?;

		// Lengths
		writeln!(w, "Lengths:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order, track_procedures, &tracks,
			|_, n, _| encode_distance(n.length.unwrap_or(0x7E00)),
			vec![0x80], ".l_"
		)?;
		self.autolist(w, &quantized_parameters,
			|_, _| vec![],
			vec![0x80]
		)?;

		// Distances
		writeln!(w, "Distances:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w, track_order, track_procedures, &tracks,
			|prev_n, n, _| {
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

	/// The notes played by the player track described by `mapping`, in time order.
	/// A note is played if its instrument is on the MIDI channel of the mapping
	/// and its key is within the note range of the mapping.
	fn track_notes(&self, mapping: &ir::MidiMapping) -> Vec<&Note> {
		self.notes.iter().filter(|n| {
			self.instruments.get(n.instr as usize)
				.is_some_and(|instr| instr.channel == mapping.channel as u16)
				&& mapping.triggered_key(mapping.channel, n.key as u8).is_some()
		}).collect()
	}

	/// Names of the instruments contributing notes to a player track, in first-use order.
	fn instrument_names(&self, notes: &[&Note]) -> Vec<String> {
		distinct(notes.iter().map(|n| n.instr)).iter()
			.filter_map(|&i| self.instruments.get(i as usize))
			.map(|instr| instr.name.clone())
			.collect()
	}

	/// Names of the tracks contributing notes to a player track, in first-use order.
	fn track_names(&self, notes: &[&Note]) -> Vec<String> {
		distinct(notes.iter().map(|n| n.track)).iter()
			.filter_map(|&t| self.tracks.get(t as usize))
			.map(|track| track.name.clone())
			.collect()
	}

	/// Whether the instrument name is redundant in the label of a player track:
	/// it is when the notes all come from one track and that track holds notes
	/// from just one instrument, so the track name already identifies it.
	fn instrument_is_implied_by_track(&self, notes: &[&Note]) -> bool {
		match distinct(notes.iter().map(|n| n.track))[..] {
			[track] => distinct(self.notes.iter()
				.filter(|n| n.track == track)
				.map(|n| n.instr)).len() == 1,
			_ => false,
		}
	}

	fn notelist<F>(&self, w: &mut dyn Write, track_order: &[ir::MidiMapping], track_procedures: &[&str],
			tracks: &[Vec<&Note>], mut datafunc: F, trackterm: Vec<u8>, prefix: &str) -> std::io::Result<()>
	where F: FnMut(Option<&Note>, &Note, &ir::MidiMapping) -> Vec<u8> {

		for (i, mapping) in track_order.iter().enumerate() {
			let notes = &tracks[i];
			let procedure = track_procedures.get(i).copied();
			let instrument_names = self.instrument_names(notes);
			let track_names = self.track_names(notes);

			// Describe the track fully in a comment, and identify it in the label by
			// the played procedure plus the track and instrument names, each of which
			// is only included when the whole track agrees on it. The instrument name
			// is dropped as well when the track name already implies it.
			let label_instrument_names: &[String] = if self.instrument_is_implied_by_track(notes) {
				&[]
			} else {
				&instrument_names
			};
			writeln!(w, "\t; {}", describe_track(mapping, procedure, &instrument_names, &track_names))?;
			writeln!(w, "{}:", label_for_track(prefix, i, procedure, &track_names, label_instrument_names))?;

			let mut prev_n: Option<&Note> = None;
			let mut last_songpos: Option<u32> = None;
			let mut pat_data = Vec::new();
			for n in notes {
				let trigger_new_line = if let Some(lp) = last_songpos { n.songpos != lp } else { true };

				if trigger_new_line {
					Self::dataline(w, &pat_data)?;
					pat_data.clear();
					writeln!(w, "\t; Position {}, pattern {}", n.songpos, n.pat)?;
					last_songpos = Some(n.songpos);
				}

				pat_data.extend(datafunc(prev_n, n, mapping));
				prev_n = Some(n);
			}
			Self::dataline(w, &pat_data)?;
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

#[cfg(test)]
mod tests {
	use super::*;

	const SAMPLE_RATE: f32 = 44100.0;

	fn instrument(name: &str, channel: u16) -> Instrument {
		Instrument { name: name.to_string(), channel }
	}

	fn track(name: &str) -> Track {
		Track { name: name.to_string(), labelname: labelify(name) }
	}

	fn note(line: u32, track: u16, column: u16, instr: u16, key: u16, velocity: u16, length: u32) -> Note {
		Note { track, column, line, length: Some(length), songpos: 0, pat: 0, patline: line as u16, instr, key, velocity }
	}

	fn mapping(channel: u8, start: u8, end: u8) -> ir::MidiMapping {
		ir::MidiMapping { channel, start, end, transpose_to: start }
	}

	/// A track holding two instruments, an instrument whose notes are spread over
	/// two tracks, two instruments sharing a MIDI channel, and a track holding a
	/// single instrument.
	fn mixed_music() -> Music {
		Music {
			tracks: vec![track("Track A"), track("Track B"), track("Track C")],
			instruments: vec![instrument("lead", 0), instrument("bass", 1), instrument("pad", 0),
				instrument("drums", 2)],
			notes: vec![
				note(0, 0, 0, 0, 60, 100, 4),
				note(1, 2, 0, 3, 36, 127, 1),
				note(2, 1, 0, 0, 62, 90, 2),
				note(4, 0, 1, 1, 40, 80, 4),
				note(6, 1, 0, 2, 64, 70, 2),
			],
			length: 8,
			ticklength: 0.1,
			autos: vec![],
		}
	}

	fn export(music: &Music, track_order: &[ir::MidiMapping], track_procedures: &[&str]) -> String {
		let mut out = vec![];
		music.export(&mut out, SAMPLE_RATE, track_order, track_procedures, 0, 16).unwrap();
		String::from_utf8(out).unwrap()
	}

	#[test]
	fn notes_of_a_channel_are_gathered_across_tracks_and_instruments() {
		let music = mixed_music();
		let out = export(&music, &[mapping(0, 0, 127), mapping(1, 0, 127)], &["synth", "bassline"]);

		// Channel 1 collects the notes of 'lead' (in both tracks) and 'pad', in time order.
		assert!(out.contains("\t; Channel 1, keys 0-127: synth, instruments 'lead', 'pad', tracks 'Track A', 'Track B'\n.v_0_synth:\n"), "{out}");
		assert!(out.contains(".v_0_synth:\n\t; Position 0, pattern 0\n\tdb\t100,90,70\n"), "{out}");
		assert!(out.contains(".k_0_synth:\n\t; Position 0, pattern 0\n\tdb\t60,62,64\n"), "{out}");
		assert!(out.contains(".l_0_synth:\n\t; Position 0, pattern 0\n\tdb\t4,2,2\n"), "{out}");
		// Distances are cumulative within the track: lines 0, 2, 6.
		assert!(out.contains(".d_0_synth:\n\t; Position 0, pattern 0\n\tdb\t0,2,4\n"), "{out}");

		// Channel 2 draws a single instrument from a single track, but 'Track A' also
		// holds 'lead', so the instrument name is needed to identify the track.
		assert!(out.contains("\t; Channel 2, keys 0-127: bassline, instrument 'bass', track 'Track A'\n.v_1_bassline_TrackA_bass:\n"), "{out}");
		assert!(out.contains(".k_1_bassline_TrackA_bass:\n\t; Position 0, pattern 0\n\tdb\t40\n"), "{out}");
		assert!(out.contains(".d_1_bassline_TrackA_bass:\n\t; Position 0, pattern 0\n\tdb\t4\n"), "{out}");
	}

	#[test]
	fn the_instrument_name_is_omitted_when_its_track_holds_only_it() {
		let music = mixed_music();
		let out = export(&music, &[mapping(2, 0, 127)], &["beat"]);

		// 'Track C' holds nothing but 'drums', so naming the track is enough.
		assert!(out.contains("\t; Channel 3, keys 0-127: beat, instrument 'drums', track 'Track C'\n.v_0_beat_TrackC:\n"), "{out}");
		assert!(out.contains(".k_0_beat_TrackC:\n\t; Position 0, pattern 0\n\tdb\t36\n"), "{out}");
	}

	#[test]
	fn note_ranges_split_a_channel_into_separate_player_tracks() {
		let music = mixed_music();
		// Two player tracks on channel 1, distinguished by note range.
		let out = export(&music, &[mapping(0, 0, 61), mapping(0, 62, 127)], &["low", "high"]);

		// Only 'lead' at key 60 is in range of the first mapping.
		assert!(out.contains("\t; Channel 1, keys 0-61: low, instrument 'lead', track 'Track A'\n.v_0_low_TrackA_lead:\n"), "{out}");
		assert!(out.contains(".k_0_low_TrackA_lead:\n\t; Position 0, pattern 0\n\tdb\t60\n"), "{out}");
		// The second mapping takes the remaining two notes: two instruments, but a single track.
		assert!(out.contains("\t; Channel 1, keys 62-127: high, instruments 'lead', 'pad', track 'Track B'\n.v_1_high_TrackB:\n"), "{out}");
		assert!(out.contains(".k_1_high_TrackB:\n\t; Position 0, pattern 0\n\tdb\t62,64\n"), "{out}");
	}

	#[test]
	fn transposition_is_applied_to_the_keys_and_shown_in_the_description() {
		let music = mixed_music();
		let out = export(&music, &[ir::MidiMapping { channel: 0, start: 60, end: 127, transpose_to: 12 }], &["low"]);

		assert!(out.contains("\t; Channel 1, keys 60-127 -> 12: low, instruments 'lead', 'pad', tracks 'Track A', 'Track B'\n.v_0_low:\n"), "{out}");
		assert!(out.contains(".k_0_low:\n\t; Position 0, pattern 0\n\tdb\t12,14,16\n"), "{out}");
	}

	#[test]
	fn a_player_track_without_notes_still_gets_a_label() {
		let music = mixed_music();
		let out = export(&music, &[mapping(5, 0, 127)], &["silent"]);

		assert!(out.contains("\t; Channel 6, keys 0-127: silent, no notes\n.v_0_silent:\n\tdb\t128\n"), "{out}");
	}
}
