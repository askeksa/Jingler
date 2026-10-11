//! The MIDI mappings of a program, written the way Zing writes them.

use ir::MidiMapping;

/// One instrument call of a program and its MIDI mapping.
#[derive(Clone, Debug, PartialEq)]
pub struct MappingRow {
	pub mapping: MidiMapping,
	pub instrument: String,
}

impl MappingRow {
	/// The channel as written in Zing, from 1 to 16.
	pub fn channel(&self) -> u8 {
		self.mapping.channel + 1
	}

	/// The key range and transposition in Zing syntax, such as
	/// `{C-4..C-6/C-3}`, or `None` for a call that plays every key as itself.
	pub fn keys(&self) -> Option<String> {
		let MidiMapping { start, end, transpose_to, .. } = self.mapping;
		if start == 0 && end == 127 && transpose_to == start {
			return None;
		}
		let range = if start == end {
			key_name(start)
		} else if end == 127 {
			format!("{}..", key_name(start))
		} else if start == 0 {
			format!("..{}", key_name(end))
		} else {
			format!("{}..{}", key_name(start), key_name(end))
		};
		Some(if transpose_to == start {
			format!("{{{range}}}")
		} else {
			format!("{{{range} / {} ({:+})}}", key_name(transpose_to), self.transposition())
		})
	}

	/// The keys as MIDI numbers, for a tooltip.
	pub fn key_numbers(&self) -> String {
		let MidiMapping { start, end, transpose_to, .. } = self.mapping;
		let played = if transpose_to == start {
			String::new()
		} else {
			format!(", played as {}–{}", transpose_to, end as i16 + self.transposition())
		};
		format!("MIDI keys {start}–{end}{played}")
	}

	/// How many semitones the keys are moved.
	fn transposition(&self) -> i16 {
		self.mapping.transpose_to as i16 - self.mapping.start as i16
	}
}

/// The instrument calls of a program, ordered by channel and then by the
/// first key of their range.
pub fn mapping_rows(program: &ir::Program) -> Vec<MappingRow> {
	let names = program.track_procedure_names();
	let mut rows: Vec<MappingRow> = program.track_order.iter().enumerate().map(|(track, &mapping)| {
		let instrument = names.get(track).unwrap_or(&"?").to_string();
		MappingRow { mapping, instrument }
	}).collect();
	rows.sort_by_key(|row| (row.mapping.channel, row.mapping.start));
	rows
}

/// A key's name in Zing syntax, where key 48 is `C-4`.
pub fn key_name(key: u8) -> String {
	const NAMES: [&str; 12] = ["C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-"];
	format!("{}{}", NAMES[key as usize % 12], key / 12)
}

#[cfg(test)]
mod tests {
	use super::*;
	use ir::{Instruction, Procedure, ProcedureKind, Program, Scope};

	fn row(start: u8, end: u8, transpose_to: u8) -> MappingRow {
		MappingRow {
			mapping: MidiMapping { channel: 0, start, end, transpose_to },
			instrument: "bass".to_string(),
		}
	}

	#[test]
	fn key_names_follow_zing() {
		assert_eq!(key_name(48), "C-4");
		assert_eq!(key_name(69), "A-5");
		assert_eq!(key_name(61), "C#5");
		assert_eq!(key_name(0), "C-0");
	}

	#[test]
	fn keys_are_written_like_zing() {
		assert_eq!(row(0, 127, 0).keys(), None);
		assert_eq!(row(0, 47, 0).keys().unwrap(), "{..B-3}");
		assert_eq!(row(48, 127, 48).keys().unwrap(), "{C-4..}");
		assert_eq!(row(48, 72, 48).keys().unwrap(), "{C-4..C-6}");
		assert_eq!(row(60, 60, 60).keys().unwrap(), "{C-5}");
		assert_eq!(row(48, 72, 36).keys().unwrap(), "{C-4..C-6 / C-3 (-12)}");
		assert_eq!(row(0, 127, 2).keys().unwrap(), "{C-0.. / D-0 (+2)}");
	}

	#[test]
	fn key_numbers_show_the_played_keys() {
		assert_eq!(row(48, 72, 48).key_numbers(), "MIDI keys 48–72");
		assert_eq!(row(48, 72, 36).key_numbers(), "MIDI keys 48–72, played as 36–60");
	}

	#[test]
	fn rows_are_sorted_by_channel_and_range() {
		let procedure = |name: &str, kind, code| Procedure {
			name: name.to_string(), kind, inputs: vec![], outputs: vec![], code,
		};
		let mapping = |channel, start, end| MidiMapping { channel, start, end, transpose_to: start };
		let program = Program {
			parameters: vec![],
			procedures: vec![
				procedure("main", ProcedureKind::Module { scope: Scope::Dynamic }, vec![
					Instruction::PlayInstrument(1, 2),
					Instruction::PlayInstrument(3, 4),
					Instruction::PlayInstrument(1, 2),
				]),
				procedure("bass", ProcedureKind::Instrument { scope: Scope::Static }, vec![]),
				procedure("bass", ProcedureKind::Instrument { scope: Scope::Dynamic }, vec![]),
				procedure("lead", ProcedureKind::Instrument { scope: Scope::Static }, vec![]),
				procedure("lead", ProcedureKind::Instrument { scope: Scope::Dynamic }, vec![]),
			],
			externals: vec![],
			main_static_proc_id: 0,
			main_dynamic_proc_id: 0,
			track_order: vec![mapping(1, 0, 127), mapping(0, 60, 127), mapping(0, 0, 59)],
		};
		let rows: Vec<_> = mapping_rows(&program).into_iter()
			.map(|row| (row.channel(), row.instrument, row.mapping.start))
			.collect();
		assert_eq!(rows, vec![
			(1, "bass".to_string(), 0),
			(1, "lead".to_string(), 60),
			(2, "bass".to_string(), 0),
		]);
	}
}
