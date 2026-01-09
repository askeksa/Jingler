
use std::error::Error;
use std::fmt::Display;
use std::io::{Read, Write};
use std::path::Path;

use zip::{ZipArchive, result::ZipError};

#[test]
fn test_convert_renoise() {
	const SAMPLE_RATE: f32 = 44100.0;
	const PARAMETER_QUANTIZATION_LEVELS: u16 = 16;

	let xrns = std::fs::File::open("../../test/test.xrns").unwrap();
	let mut archive = ZipArchive::new(xrns).unwrap();
	let mut song = archive.by_name("Song.xml").unwrap();

	let music = convert_renoise_song(
		&mut song,
		SAMPLE_RATE,
		PARAMETER_QUANTIZATION_LEVELS,
		4, vec![0, 1, 8, 3, 9, 4, 2, 6, 7, 5]
	).unwrap();

	let mut out = vec![];
	music.export(&mut out).unwrap();

	let expected = std::fs::read_to_string("../../test/expected.asm").unwrap();
	let actual = String::from_utf8(out).unwrap();
	std::fs::write("../../test/actual.asm", &actual).unwrap();
	assert_eq!(expected, actual);
}

pub fn convert_renoise_file(input: &impl AsRef<Path>,
		sample_rate: f32, parameter_quantization_levels: u16,
		parameter_count: u16, track_order: Vec<u16>) -> Result<Music, ConvertError> {
	let xrns = std::fs::File::open(input.as_ref())?;
	let mut archive = ZipArchive::new(xrns)?;
	let mut song = archive.by_name("Song.xml")?;
	convert_renoise_song(
		&mut song,
		sample_rate,
		parameter_quantization_levels,
		parameter_count,
		track_order
	)
}

pub fn convert_renoise_song(song: &mut dyn Read,
		sample_rate: f32, parameter_quantization_levels: u16,
		parameter_count: u16, track_order: Vec<u16>) -> Result<Music, ConvertError> {
	let mut content = String::new();
	song.read_to_string(&mut content)?;
	let doc = roxmltree::Document::parse(&content)
		.map_err(|e| ConvertError::General { message: e.to_string() })?;

	let x = XmlNode::wrap(doc.root());
	let xsong = x.child("RenoiseSong");

	make_music(&xsong, sample_rate, &track_order, parameter_count, parameter_quantization_levels)
}

// Data Structures

#[derive(Clone, Debug)]
pub struct Instrument {
	pub number: u16,
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
	pub off: bool,
	pub tone: Option<u16>,
	pub instr: u16,
	pub velocity: u16,
}

#[derive(Clone, Debug)]
pub struct AutomationPoint {
	pub line: u32,
	pub value: u16,
}

#[derive(Clone, Debug)]
pub struct Track {
	pub number: u16,
	pub name: String,
	pub instr: u16,
	pub notes: Vec<Note>,
	pub labelname: String,
}

#[derive(Clone, Debug)]
pub struct Music {
	pub sample_rate: f32,
	pub tracks: Vec<Track>,
	pub instruments: Vec<Instrument>,
	pub length: u32,
	pub ticklength: f32,
	pub autos: Vec<Vec<AutomationPoint>>,
	pub track_order: Vec<Option<usize>>, // Index into self.tracks
}

// XML Wrapper

struct XmlNode<'a> {
	nodes: Vec<roxmltree::Node<'a, 'a>>,
}

impl<'a> XmlNode<'a> {
	fn new(nodes: Vec<roxmltree::Node<'a, 'a>>) -> Self {
		Self { nodes }
	}

	fn wrap(node: roxmltree::Node<'a, 'a>) -> Self {
		Self::new(vec![node])
	}

	fn at(&self, index: usize) -> Self {
		Self::wrap(self.nodes[index])
	}

	fn child(&self, name: &str) -> XmlNode<'a> {
		let mut children = Vec::new();
		for node in &self.nodes {
			for child in node.children() {
				if child.has_tag_name(name) {
					children.push(child);
				}
			}
		}
		XmlNode::new(children)
	}

	fn attr(&self, name: &str) -> String {
		let mut s = String::new();
		for node in &self.nodes {
			if let Some(val) = node.attribute(name) {
				s.push_str(val);
			}
		}
		s
	}

	fn text(&self) -> String {
		let mut s = String::new();
		for node in &self.nodes {
			if let Some(text) = node.text() {
				s.push_str(text);
			}
		}
		s
	}

	fn is_empty(&self) -> bool {
		self.nodes.is_empty()
	}

	fn len(&self) -> usize {
		self.nodes.len()
	}

	fn iter(&self) -> impl Iterator<Item = XmlNode<'a>> + '_ {
		self.nodes.iter().map(|n| XmlNode::wrap(*n))
	}
}

fn make_music<'a>(xsong: &XmlNode<'a>, sample_rate: f32, track_order: &[u16], num_parameters: u16, quantization_levels: u16) -> Result<Music, ConvertError> {
	let xgsd = xsong.child("GlobalSongData");
	let playback_version = xgsd.child("PlaybackEngineVersion").text();
	let playback_version: i32 = if playback_version.is_empty() { 0 } else { playback_version.parse().unwrap_or(0) };

	let beats_per_min: f32 = xgsd.child("BeatsPerMin").text().parse().unwrap_or(0.0);

	let ticklength = if playback_version >= 4 {
		let lines_per_beat: f32 = xgsd.child("LinesPerBeat").text().parse().unwrap_or(0.0);
		let lines_per_minute = beats_per_min * lines_per_beat;
		60.0 / lines_per_minute
	} else {
		let ticks_per_line: f32 = xgsd.child("TicksPerLine").text().parse().unwrap_or(0.0);
		let lines_per_minute = beats_per_min * 24.0 / ticks_per_line;
		60.0 / lines_per_minute
	};

	let (tracks, instruments) = make_tracks(xsong, ticklength)?;

	let mut autos = Vec::new();
	for i in 0..num_parameters {
		autos.push(extract_automation(xsong, i + 1, quantization_levels));
	}

	let xpositions = xsong.child("PatternSequence").child("PatternSequence").child("Pattern");
	let xpositions = if !xpositions.is_empty() { xpositions } else { xsong.child("PatternSequence").child("SequenceEntries").child("SequenceEntry").child("Pattern") };

	let xpatterns = xsong.child("PatternPool").child("Patterns").child("Pattern");

	let mut length: u32 = 0;
	for xpos in xpositions.iter() {
		let patn: usize = xpos.text().parse().unwrap_or(0);
		if patn < xpatterns.len() {
			let xpat = xpatterns.at(patn);
			let nlines: u32 = xpat.child("NumberOfLines").text().parse().unwrap_or(0);
			length += nlines;
		}
	}

	let mut channel_map: Vec<Option<usize>> = vec![None; 16];
	for (i, track) in tracks.iter().enumerate() {
		let channel = instruments[track.instr as usize].channel as usize;
		if channel < 16 && channel_map[channel].is_some() {
			let existing = &tracks[channel_map[channel].unwrap()];
			return Err(ConvertError::Tracks { track1: track.name.clone(), track2: existing.name.clone(), message: "uses same channel".to_string() });
		}
		if channel < 16 {
			channel_map[channel] = Some(i);
		}
	}

	let mut final_track_order = Vec::new();
	for &idx in track_order {
		if (idx as usize) < 16 {
			final_track_order.push(channel_map[idx as usize]);
		} else {
			final_track_order.push(None);
		}
	}

	Ok(Music {
		sample_rate,
		tracks,
		instruments,
		length,
		ticklength,
		autos,
		track_order: final_track_order,
	})
}

fn make_tracks<'a>(xsong: &XmlNode<'a>, _ticklength: f32) -> Result<(Vec<Track>, Vec<Instrument>), ConvertError> {
	let mut instruments = Vec::new();
	for (i, xinst) in xsong.child("Instruments").child("Instrument").iter().enumerate() {
		let channel: u16 = xinst.child("PluginGenerator").child("Channel").text().parse().unwrap_or(0);
		let name = xinst.child("Name").text();
		instruments.push(Instrument {
			number: i as u16,
			name,
			channel,
		});
	}

	for xgrouptrack in xsong.child("Tracks").child("SequencerGroupTrack").iter() {
		let tname = xgrouptrack.child("Name").text();
		let xdevices = xgrouptrack.child("FilterDevices").child("Devices");
		for xmixer in xdevices.child("GroupTrackMixerDevice").iter() {
			if is_active(&xmixer) {
				let vol: f32 = xmixer.child("Volume").child("Value").text().parse().unwrap_or(1.0);
				let post_vol: f32 = xmixer.child("PostVolume").child("Value").text().parse().unwrap_or(1.0);
				if vol != 1.0 || post_vol != 1.0 {
					return Err(ConvertError::GroupTrack { track: tname.clone(), message: "has non-zero volume".into() });
				}
			}
		}
		for xsend in xdevices.child("SendDevice").iter() {
			if is_active(&xsend) {
				return Err(ConvertError::GroupTrack { track: tname, message: "uses Send".into() });
			}
		}
	}

	let mut tracks = Vec::new();
	for (tr, xtrack) in xsong.child("Tracks").child("SequencerTrack").iter().enumerate() {
		if xtrack.child("State").text() != "Active" {
			continue;
		}

		let tname = xtrack.child("Name").text();
		let ncols: usize = xtrack.child("NumberOfVisibleNoteColumns").text().parse().unwrap_or(1);

		let xdevices = xtrack.child("FilterDevices").child("Devices");
		for xsend in xdevices.child("SendDevice").iter() {
			if is_active(&xsend) {
				return Err(ConvertError::Track { track: tname, message: "uses Send".into() });
			}
		}

		let mut notes = Vec::new();
		let mut track_instrs = Vec::new();

		let col_states = xtrack.child("NoteColumnStates").child("NoteColumnState");
		for column in 0..ncols {
			let col_state = if column < col_states.len() { Some(col_states.at(column)) } else { None };
			let is_active = if let Some(cs) = col_state { cs.text() == "Active" } else { false };
			if !is_active { continue; }

			let mut col_notes = extract_track_notes(xsong, tr, column, &tname)?;
			notes.append(&mut col_notes);

			for note in &notes {
				if !note.off && !track_instrs.contains(&note.instr) {
					track_instrs.push(note.instr);
				}
			}
		}

		for instr in track_instrs {
			let mut instr_notes = filter_track_notes(&tname, &notes, instr);
			compute_note_lengths(&mut instr_notes, &tname)?;
			instr_notes.sort_by_key(|n| n.line);

			let labelname: String = tname.chars().filter(|c| c.is_alphanumeric() || *c == '_').collect();

			tracks.push(Track {
				number: tr as u16,
				name: tname.clone(),
				instr,
				notes: instr_notes,
				labelname,
			});
		}
	}

	Ok((tracks, instruments))
}

fn extract_track_notes<'a>(xsong: &XmlNode<'a>, tr: usize, column: usize, tname: &str) -> Result<Vec<Note>, ConvertError> {
	let xsequence = xsong.child("PatternSequence").child("PatternSequence");
	let xsequence = if !xsequence.is_empty() { xsequence } else { xsong.child("PatternSequence").child("SequenceEntries").child("SequenceEntry") };

	let xpatterns = xsong.child("PatternPool").child("Patterns").child("Pattern");

	let mut notes = Vec::new();
	let mut pattern_top = 0;
	let mut prev_instr: Option<u16> = None;

	for (posn, xseq) in xsequence.iter().enumerate() {
		let patn: usize = xseq.child("Pattern").text().parse().unwrap_or(0);
		let xpat = xpatterns.at(patn);
		let nlines: u32 = xpat.child("NumberOfLines").text().parse().unwrap_or(0);

		let muted = xseq.child("MutedTracks").child("MutedTrack").iter().any(|mt| {
			mt.text().parse::<usize>().unwrap_or(usize::MAX) == tr
		});

		if muted {
			notes.push(Note {
				column: column as u16, line: pattern_top, length: None, songpos: posn as u32, pat: patn as u16, patline: 0,
				off: true, tone: None, instr: 0, velocity: 127
			});
		} else {
			let tracks = xpat.child("Tracks").child("PatternTrack");
			if tr < tracks.len() {
				let xtrack = tracks.at(tr);
				for xline in xtrack.child("Lines").child("Line").iter() {
					let index: u16 = xline.attr("index").parse().unwrap_or(0);
					if (index as u32) < nlines {
						let line = pattern_top + index as u32;
						let xcols = xline.child("NoteColumns").child("NoteColumn");
						if column < xcols.len() {
							let xcol = xcols.at(column);
							let note_str = xcol.child("Note").text();

							if !note_str.is_empty() && note_str != "---" {
								let instr_str = xcol.child("Instrument").text();
								let mut instr = instr_str.clone();
								if (instr == ".." || instr.is_empty()) && note_str != "OFF" {
									if let Some(pi) = prev_instr {
										instr = format!("{:02X}", pi);
									} else {
										return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: "Unspecified instrument".into() });
									}
								}
								let instr_val = if !instr.is_empty() && instr != ".." { u16::from_str_radix(&instr, 16).unwrap_or(0) } else { 0 };
								if !instr.is_empty() && instr != ".." {
									prev_instr = Some(instr_val);
								}

								// Parse velocity
								let vol_str = xcol.child("Volume").text();
								let velocity = if vol_str.is_empty() || vol_str == ".." { 127 } else { u16::from_str_radix(&vol_str, 16).unwrap_or(127) };
								if velocity > 128 && note_str != "OFF" {
									return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: "Illegal velocity value".into() });
								}

								let mut note = parse_note_struct(tname, column, line, posn as u32, patn as u16, index, &note_str, instr_val, velocity);
								if note.velocity == 0 {
									note.off = true;
								}
								notes.push(note);
							}

							// Check columns
							let check = |x: &str, allow_zero: bool, msg: &str| -> Result<(), ConvertError> {
								if !x.is_empty() && x != ".." && (!allow_zero || x != "00") {
									return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: msg.to_string() });
								}
								Ok(())
							};
							check(&xcol.child("Panning").text(), false, "Panning column used")?;
							check(&xcol.child("Delay").text(), true, "Delay column used")?;

							for xeff in xline.child("EffectColumns").child("EffectColumn").iter() {
								check(&xeff.child("Number").text(), true, "Effect column used")?;
							}
						}
					}
				}
			}
		}
		pattern_top += nlines;
	}

	// Add initial OFF and remove redundant OFFs
	let mut notes2 = Vec::new();
	let mut off = if !notes.is_empty() && notes[0].line == 0 { false } else {
		notes2.push(Note { column: column as u16, line: 0, length: None, songpos: 0, pat: 0, patline: 0, off: true, tone: None, instr: 0, velocity: 127 });
		true
	};

	for n in notes {
		if n.off {
			if !off {
				notes2.push(n);
				off = true;
			}
		} else {
			notes2.push(n);
			off = false;
		}
	}

	if let Some(last) = notes2.last() {
		if !last.off {
			return Err(ConvertError::Note {
				track: tname.to_string(), column: column as u16, pattern: last.pat, line: last.patline, message: "Note not terminated (insert OFF)".into()
			});
		}
	}

	Ok(notes2)
}

fn parse_note_struct(_tname: &str, column: usize, line: u32, songpos: u32, pat: u16, patline: u16, note_str: &str, instr: u16, velocity: u16) -> Note {
	if note_str == "OFF" {
		Note { column: column as u16, line, length: None, songpos, pat, patline, off: true, tone: None, instr: 0, velocity: 0 }
	} else {
		let chars: Vec<char> = note_str.chars().collect();
		let octave = chars[2].to_digit(10).unwrap_or(0) as u16;
		let notebase = match chars[0] {
			'C' => 0, 'D' => 2, 'E' => 4, 'F' => 5, 'G' => 7, 'A' => 9, 'B' => 11, _ => 0
		};
		let sharp = if chars[1] == '#' { 1 } else { 0 };
		let tone = octave * 12 + notebase + sharp;
		Note { column: column as u16, line, length: None, songpos, pat, patline, off: false, tone: Some(tone), instr, velocity }
	}
}

fn filter_track_notes(_tname: &str, notes: &[Note], instr: u16) -> Vec<Note> {
	let mut notes2 = Vec::new();
	let mut off = false;

	for n in notes {
		if n.off || n.instr != instr {
			if !off {
				notes2.push(Note {
					column: n.column, line: n.line, length: None, songpos: n.songpos, pat: n.pat, patline: n.patline,
					off: true, tone: None, instr: 0, velocity: 127
				});
				off = true;
			}
		} else {
			notes2.push(n.clone());
			off = false;
		}
	}
	notes2
}

fn compute_note_lengths(notes: &mut [Note], tname: &str) -> Result<(), ConvertError> {
	for i in 1..notes.len() {
		let prev_line = notes[i-1].line;
		let curr_line = notes[i].line;

		if !notes[i-1].off {
			if curr_line == prev_line {
				return Err(ConvertError::Note { track: tname.to_string(), column: notes[i].column, pattern: notes[i].pat, line: notes[i].patline, message: "Zero length".into() });
			}
			notes[i-1].length = Some(curr_line - prev_line);
		}
	}
	Ok(())
}

fn extract_automation<'a>(xsong: &XmlNode<'a>, id: u16, quantization_levels: u16) -> Vec<AutomationPoint> {
	let xsequence = xsong.child("PatternSequence").child("PatternSequence");
	let xsequence = if !xsequence.is_empty() { xsequence } else { xsong.child("PatternSequence").child("SequenceEntries").child("SequenceEntry") };
	let xpatterns = xsong.child("PatternPool").child("Patterns").child("Pattern");

	let mut points: Vec<AutomationPoint> = Vec::new();
	let mut line = 0;

	let add = |pline: u32, pvalue: u16, points: &mut Vec<AutomationPoint>| {
		if points.is_empty() || pline != points.last().unwrap().line || pvalue != points.last().unwrap().value {
			if points.len() >= 2 && points[points.len()-2].value == pvalue && points.last().unwrap().value == pvalue {
				let last = points.last_mut().unwrap();
				last.line = pline;
			} else {
				points.push(AutomationPoint { line: pline, value: pvalue });
			}
		}
	};

	for xseq in xsequence.iter() {
		let patn: usize = xseq.child("Pattern").text().parse().unwrap_or(0);
		let xpat = xpatterns.at(patn);
		let nlines: u32 = xpat.child("NumberOfLines").text().parse().unwrap_or(0);

		for xtrack in xpat.child("Tracks").child("PatternTrack").iter() {
			for xenvelope in xtrack.child("Automations").child("Envelopes").child("Envelope").iter() {
				let param_idx: u16 = xenvelope.child("ParameterIndex").text().parse().unwrap_or(0);
				if param_idx == id {
					let length: f32 = xenvelope.child("Envelope").child("Length").text().parse().unwrap_or(1.0);
					let mut first = true;
					for xpoint in xenvelope.child("Envelope").child("Points").child("Point").iter() {
						let text = xpoint.text(); // e.g., "0.5,0.1"
						let parts: Vec<&str> = text.split(',').collect();
						if parts.len() >= 2 {
								let x: f32 = parts[0].parse().unwrap_or(0.0);
								let y: f32 = parts[1].parse().unwrap_or(0.0);

								let pline = line + ((x * nlines as f32).round() / length as f32).floor() as u32;
								let pvalue = (y * quantization_levels as f32).round() as u16;

								if first && pline > line {
									add(line, pvalue, &mut points);
								}
								add(pline, pvalue, &mut points);
								first = false;
						}
					}
				}
			}
		}
		line += nlines;

		// flush
		if !points.is_empty() && points.last().unwrap().line < line {
			let val = points.last().unwrap().value;
			add(line, val, &mut points);
		}
	}
	points
}

fn is_active(xdevice: &XmlNode) -> bool {
	if xdevice.is_empty() { return false; }
	let val = xdevice.child("IsActive").child("Value").text();
	if !val.is_empty() {
		val.parse::<f32>().unwrap_or(0.0) != 0.0
	} else {
		xdevice.child("IsActive").text() == "true"
	}
}

// Export logic
impl Music {
	pub fn export(&self, w: &mut dyn Write) -> std::io::Result<()> {
		let sample_rate = self.sample_rate;
		let spt = (self.ticklength * sample_rate).round() as u32;
		let total_samples = (self.length as f32 * self.ticklength * sample_rate) as u64;

		let roundup = |v: u64| (v & !0xFFFF) + 0x10000;

		writeln!(w, "%define SAMPLE_RATE {:.0}", sample_rate)?;
		writeln!(w)?;
		writeln!(w, "%define NUM_TRACKS {}", self.track_order.len())?;
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
		self.notelist(w,
			|_, n| vec![n.velocity as u8],
			vec![0x80], ".v_"
		)?;
		self.autolist(w, |_, _| vec![], vec![0x80])?;

		// Keys
		writeln!(w, "Keys:\n\tdd\t1")?;
		self.notelist(w,
			|_, n| vec![n.tone.unwrap() as u8],
			vec![0x80], ".k_"
		)?;
		self.autolist(w, |_, p| vec![p.value as u8], vec![0x80])?;

		// Lengths
		writeln!(w, "Lengths:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w,
			|_, n| encode_distance(n.length.unwrap()),
			vec![0x80], ".l_"
		)?;
		self.autolist(w, |_, _| vec![], vec![0x80])?;

		// Distances
		writeln!(w, "Distances:\n\tdd\tSAMPLES_PER_TICK")?;
		self.notelist(w,
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

	fn notelist<F>(&self, w: &mut dyn Write, mut datafunc: F, trackterm: Vec<u8>, prefix: &str) -> std::io::Result<()>
	where F: FnMut(Option<&Note>, &Note) -> Vec<u8> {

		for (i, track_opt) in self.track_order.iter().enumerate() {
			if let Some(track_idx) = track_opt {
				let track = &self.tracks[*track_idx];
				writeln!(w, "\t; {}", track.name)?;
				writeln!(w, "{}{}_{}_{}:", prefix, i, track.labelname, track.instr)?;

				let mut prev_n: Option<&Note> = None;
				let mut last_songpos: Option<u32> = None;
				let mut pat_data = Vec::new();
				for n in &track.notes {
					let trigger_new_line = if let Some(lp) = last_songpos { n.songpos != lp } else { !n.off };

					if trigger_new_line {
						Self::dataline(w, &pat_data)?;
						pat_data.clear();
						writeln!(w, "\t; Position {}, pattern {}", n.songpos, n.pat)?;
						last_songpos = Some(n.songpos);
					}

					if !n.off {
						pat_data.extend(datafunc(prev_n, n));
						prev_n = Some(n);
					}
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

#[derive(Debug)]
pub enum ConvertError {
	Note {
		track: String,
		column: u16,
		pattern: u16,
		line: u16,
		message: String,
	},
	GroupTrack {
		track: String,
		message: String,
	},
	Track {
		track: String,
		message: String,
	},
	Tracks {
		track1: String,
		track2: String,
		message: String,
	},
	General {
		message: String,
	},
	Io {
		cause: std::io::Error,
	},
}

impl Display for ConvertError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ConvertError::Note { track, column, pattern, line, message } => {
				write!(f, "Track '{track}' column {column} pattern {pattern} line {line}: {message}")
			},
			ConvertError::GroupTrack { track, message } => {
				write!(f, "Group track '{track}' {message}")
			},
			ConvertError::Track { track, message } => {
				write!(f, "Track '{track}' {message}")
			},
			ConvertError::Tracks { track1, track2, message } => {
				write!(f, "Tracks '{track1}' and '{track2}' {message}")
			},
			ConvertError::General { message } => {
				write!(f, "{message}")
			},
			ConvertError::Io { cause } => {
				write!(f, "I/O error: {cause}")
			},
		}
	}
}

impl Error for ConvertError {
	fn source(&self) -> Option<&(dyn Error + 'static)> {
		match self {
			ConvertError::Io { cause } => Some(cause),
			_ => None,
		}
	}
}

impl From<std::io::Error> for ConvertError {
	fn from(err: std::io::Error) -> ConvertError {
		ConvertError::Io { cause: err }
	}
}

impl From<ZipError> for ConvertError {
	fn from(err: ZipError) -> ConvertError {
		match err {
			ZipError::Io(err) => ConvertError::Io { cause: err },
			_ => ConvertError::General { message: err.to_string() },
		}
	}
}
