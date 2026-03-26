
use std::error::Error;
use std::fmt::Display;
use std::io::Read;
use std::path::Path;

use zip::{ZipArchive, result::ZipError};

use crate::*;

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
