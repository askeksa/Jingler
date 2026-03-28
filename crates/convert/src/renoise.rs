
use std::error::Error;
use std::fmt::Display;
use std::io::Read;
use std::path::Path;

use zip::{ZipArchive, result::ZipError};

use crate::*;
use crate::xml::{XmlDocument, XmlNode};

#[test]
fn test_convert_renoise() {
	const SAMPLE_RATE: f32 = 44100.0;
	const PARAMETER_QUANTIZATION_LEVELS: u16 = 16;
	const TRACK_ORDER: [u16; 10] = [0, 1, 8, 3, 9, 4, 2, 6, 7, 5];
	const NUM_PARAMETERS: usize = 4;

	let xrns = std::fs::File::open("../../test/test.xrns").unwrap();
	let mut archive = ZipArchive::new(xrns).unwrap();
	let mut song = archive.by_name("Song.xml").unwrap();

	let music = convert_renoise_song(&mut song, PARAMETER_QUANTIZATION_LEVELS).unwrap();

	let mut out = vec![];
	music.export(&mut out, SAMPLE_RATE, &TRACK_ORDER, NUM_PARAMETERS).unwrap();

	let expected = std::fs::read_to_string("../../test/expected.asm").unwrap();
	let actual = String::from_utf8(out).unwrap();
	std::fs::write("../../test/actual.asm", &actual).unwrap();
	assert_eq!(expected, actual);
}

pub fn convert_renoise_file(input: &impl AsRef<Path>,
		parameter_quantization_levels: u16) -> Result<Music, ConvertError> {
	let xrns = std::fs::File::open(input.as_ref())?;
	let mut archive = ZipArchive::new(xrns)?;
	let mut song = archive.by_name("Song.xml")?;
	convert_renoise_song(&mut song, parameter_quantization_levels)
}

pub fn convert_renoise_song(song: &mut dyn Read,
		parameter_quantization_levels: u16) -> Result<Music, ConvertError> {
	let mut content = String::new();
	song.read_to_string(&mut content)?;
	let doc = XmlDocument::parse(&content)
		.map_err(|e| ConvertError::General { message: e.to_string() })?;

	let xsong = doc.root().child("RenoiseSong");

	make_music(&xsong, parameter_quantization_levels)
}

fn make_music(xsong: &XmlNode, quantization_levels: u16) -> Result<Music, ConvertError> {
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

	let autos = extract_automation(xsong, quantization_levels);

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

	Ok(Music {
		tracks,
		instruments,
		length,
		ticklength,
		autos,
		channel_map,
	})
}

fn make_tracks(xsong: &XmlNode, _ticklength: f32) -> Result<(Vec<Track>, Vec<Instrument>), ConvertError> {
	let mut instruments = Vec::new();
	for xinst in xsong.child("Instruments").child("Instrument").iter() {
		let channel: u16 = xinst.child("PluginGenerator").child("Channel").text().parse().unwrap_or(0);
		let name = xinst.child("Name").text();
		instruments.push(Instrument {
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
		}

		for note in &notes {
			if !track_instrs.contains(&note.instr) {
				track_instrs.push(note.instr);
			}
		}

		for instr in track_instrs {
			let mut instr_notes: Vec<Note> = notes.iter().filter(|n| n.instr == instr).cloned().collect();
			instr_notes.sort_by_key(|n| n.line);

			let labelname: String = tname.chars().filter(|c| c.is_alphanumeric() || *c == '_').collect();

			tracks.push(Track {
				name: tname.clone(),
				instr,
				notes: instr_notes,
				labelname,
			});
		}
	}

	Ok((tracks, instruments))
}

fn extract_track_notes(xsong: &XmlNode, tr: usize, column: usize, tname: &str) -> Result<Vec<Note>, ConvertError> {
	let xsequence = xsong.child("PatternSequence").child("PatternSequence");
	let xsequence = if !xsequence.is_empty() { xsequence } else { xsong.child("PatternSequence").child("SequenceEntries").child("SequenceEntry") };

	let xpatterns = xsong.child("PatternPool").child("Patterns").child("Pattern");

	let mut notes: Vec<Note> = Vec::new();
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
			// Terminate last note at top of pattern
			notes.last_mut().map(|last| last.length.get_or_insert(pattern_top - last.line));
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

							// Check illegal columns
							let check = |x: &str, allow_zero: bool, msg: &str| -> Result<(), ConvertError> {
								if x != "" && x != ".." && (!allow_zero || x != "00") {
									return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: msg.to_string() });
								}
								Ok(())
							};
							check(&xcol.child("Panning").text(), false, "Panning column used")?;
							check(&xcol.child("Delay").text(), true, "Delay column used")?;

							for xeff in xline.child("EffectColumns").child("EffectColumn").iter() {
								check(&xeff.child("Number").text(), true, "Effect column used")?;
							}

							// Skip if empty
							let note_str = xcol.child("Note").text();
							if note_str == "" || note_str == "---" { continue; }

							// Terminate last note
							notes.last_mut().map(|last| last.length.get_or_insert(line - last.line));

							// Skip if OFF
							if note_str == "OFF" { continue; }

							// Parse instrument
							let instr_str = xcol.child("Instrument").text();
							let instr = if instr_str == "" || instr_str == ".." {
								if let Some(prev_instr) = prev_instr {
									prev_instr
								} else {
									return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: "Unspecified instrument".into() });
								}
							} else {
								u16::from_str_radix(&instr_str, 16).unwrap_or(0)
							};
							prev_instr = Some(instr);

							// Parse key
							let chars: Vec<char> = note_str.chars().collect();
							let octave = chars[2].to_digit(10).unwrap_or(0) as u16;
							let notebase = match chars[0] {
								'C' => 0, 'D' => 2, 'E' => 4, 'F' => 5, 'G' => 7, 'A' => 9, 'B' => 11, _ => 0
							};
							let sharp = if chars[1] == '#' { 1 } else { 0 };
							let key = octave * 12 + notebase + sharp;

							// Parse velocity
							let vol_str = xcol.child("Volume").text();
							let velocity = if vol_str == "" || vol_str == ".." {
								127
							} else {
								u16::from_str_radix(&vol_str, 16).unwrap_or(127)
							};
							if velocity > 127 {
								return Err(ConvertError::Note { track: tname.to_string(), column: column as u16, pattern: patn as u16, line: index, message: "Illegal velocity value".into() });
							}

							let note = Note {
								column: column as u16,
								line,
								length: None,
								songpos: posn as u32,
								pat: patn as u16,
								patline: index,
								instr,
								key,
								velocity
							};
							notes.push(note);
						}
					}
				}
			}
		}
		pattern_top += nlines;
	}

	Ok(notes)
}

fn extract_automation(xsong: &XmlNode, quantization_levels: u16) -> Vec<Vec<AutomationPoint>> {
	let xsequence = xsong.child("PatternSequence").child("PatternSequence");
	let xsequence = if !xsequence.is_empty() { xsequence } else { xsong.child("PatternSequence").child("SequenceEntries").child("SequenceEntry") };
	let xpatterns = xsong.child("PatternPool").child("Patterns").child("Pattern");

	let mut parameter_points: Vec<Vec<AutomationPoint>> = Vec::new();
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
				let Ok(param_idx) = xenvelope.child("ParameterIndex").text().parse() else { continue; };
				while parameter_points.len() < param_idx {
					parameter_points.push(Vec::new());
				}
				let mut points = &mut parameter_points[param_idx - 1];

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
		line += nlines;

		// flush
		for mut points in parameter_points.iter_mut() {
			if !points.is_empty() && points.last().unwrap().line < line {
				let val = points.last().unwrap().value;
				add(line, val, &mut points);
			}
		}
	}

	parameter_points
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
