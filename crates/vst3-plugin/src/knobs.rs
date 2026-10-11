//! Assignment of the program's parameters to the plugin's knobs (ADR 0006).
//!
//! Knobs follow the parameters in declaration order. When a new program
//! arrives, a knob whose parameter is new gets the parameter's default, and a
//! parameter that already existed keeps its value, even when it moved to
//! another knob or its range changed.

use serde::{Deserialize, Serialize};

pub const NUM_KNOBS: usize = 15;

/// The parameter a knob is assigned to, as saved with the plugin state.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct KnobParameter {
	pub name: String,
	pub min: f32,
	pub max: f32,
}

impl KnobParameter {
	fn plain(&self, normalized: f32) -> f32 {
		self.min + normalized * (self.max - self.min)
	}
}

impl From<&ir::Parameter> for KnobParameter {
	fn from(param: &ir::Parameter) -> Self {
		KnobParameter { name: param.name.clone(), min: param.min, max: param.max }
	}
}

/// What the plugin knows about the parameters its knobs are assigned to.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum KnobAssignment {
	/// The state was saved before assignments were: adopt the next program's
	/// parameters without moving any knob.
	Unknown,
	/// The parameter of each knob, `None` for an unused knob.
	Known(Vec<Option<KnobParameter>>),
}

impl Default for KnobAssignment {
	/// A fresh plugin: no knob has a parameter, so all of them are new.
	fn default() -> Self {
		KnobAssignment::Known(vec![None; NUM_KNOBS])
	}
}

impl KnobAssignment {
	/// The assignment for the parameters of a program.
	pub fn of(parameters: &[ir::Parameter]) -> Self {
		KnobAssignment::Known((0..NUM_KNOBS)
			.map(|knob| parameters.get(knob).map(KnobParameter::from))
			.collect())
	}

	/// The knob moves that take the knobs from this assignment, with the
	/// normalized knob `values`, to the given parameters: `(knob, normalized
	/// value)` for each knob whose value changes.
	pub fn moves(&self, values: &[f32], parameters: &[ir::Parameter]) -> Vec<(usize, f32)> {
		let KnobAssignment::Known(old) = self else { return vec![] };
		parameters.iter().take(NUM_KNOBS).enumerate().filter_map(|(knob, param)| {
			let new = KnobParameter::from(param);
			if old.get(knob).is_some_and(|p| p.as_ref() == Some(&new)) {
				return None;
			}
			let previous = old.iter().zip(values).find_map(|(p, &value)| {
				p.as_ref().filter(|p| p.name == param.name).map(|p| p.plain(value))
			});
			let value = normalize(previous.unwrap_or(param.default), param.min, param.max);
			(values.get(knob) != Some(&value)).then_some((knob, value))
		}).collect()
	}
}

/// The knob position of a value, clamped to the range.
pub fn normalize(value: f32, min: f32, max: f32) -> f32 {
	if max == min {
		return 0.0;
	}
	((value - min) / (max - min)).clamp(0.0, 1.0)
}

/// A parameter value with about four significant digits.
pub fn format_value(value: f32) -> String {
	if value == 0.0 || !value.is_finite() {
		return format!("{value}");
	}
	let decimals = (3 - value.abs().log10().floor() as i32).clamp(0, 6) as usize;
	let text = format!("{value:.decimals$}");
	if text.contains('.') {
		text.trim_end_matches('0').trim_end_matches('.').to_string()
	} else {
		text
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn values_have_four_significant_digits() {
		assert_eq!(format_value(800.0), "800");
		assert_eq!(format_value(1234.567), "1235");
		assert_eq!(format_value(123456.0), "123456");
		assert_eq!(format_value(0.5), "0.5");
		assert_eq!(format_value(0.123456), "0.1235");
		assert_eq!(format_value(-3.0), "-3");
		assert_eq!(format_value(0.0), "0");
	}

	fn param(name: &str, min: f32, max: f32, default: f32) -> ir::Parameter {
		ir::Parameter { name: name.to_string(), min, max, default }
	}

	fn knobs(values: &[f32]) -> Vec<f32> {
		let mut knobs = vec![0.5; NUM_KNOBS];
		knobs[..values.len()].copy_from_slice(values);
		knobs
	}

	#[test]
	fn fresh_plugin_gives_every_parameter_its_default() {
		let params = [param("cutoff", 100.0, 4100.0, 900.0), param("res", 0.0, 1.0, 0.5)];
		let moves = KnobAssignment::default().moves(&knobs(&[]), &params);
		// res already sits at its default
		assert_eq!(moves, vec![(0, 0.2)]);
	}

	#[test]
	fn unchanged_parameters_keep_their_knobs() {
		let params = [param("cutoff", 100.0, 4100.0, 900.0), param("res", 0.0, 1.0, 0.5)];
		let assignment = KnobAssignment::of(&params);
		assert_eq!(assignment.moves(&knobs(&[0.9, 0.1]), &params), vec![]);
	}

	#[test]
	fn values_follow_names_when_a_parameter_is_inserted() {
		let old = [param("cutoff", 0.0, 10.0, 0.0), param("res", 0.0, 1.0, 0.0)];
		let new = [param("attack", 0.0, 10.0, 2.0), param("cutoff", 0.0, 10.0, 0.0), param("res", 0.0, 1.0, 0.0)];
		let moves = KnobAssignment::of(&old).moves(&knobs(&[0.3, 0.7]), &new);
		assert_eq!(moves, vec![(0, 0.2), (1, 0.3), (2, 0.7)]);
	}

	#[test]
	fn a_changed_range_keeps_the_value() {
		let old = [param("cutoff", 100.0, 4100.0, 100.0)];
		let new = [param("cutoff", 100.0, 8100.0, 100.0)];
		// 0.25 of 100..4100 is 1100, which is 0.125 of 100..8100
		let moves = KnobAssignment::of(&old).moves(&knobs(&[0.25]), &new);
		assert_eq!(moves, vec![(0, 0.125)]);
	}

	#[test]
	fn a_value_outside_the_new_range_is_clamped() {
		let old = [param("gain", 0.0, 10.0, 0.0)];
		let new = [param("gain", 0.0, 4.0, 0.0)];
		let moves = KnobAssignment::of(&old).moves(&knobs(&[0.8]), &new);
		assert_eq!(moves, vec![(0, 1.0)]);
	}

	#[test]
	fn a_renamed_parameter_is_new() {
		let old = [param("cutoff", 0.0, 10.0, 0.0)];
		let new = [param("brightness", 0.0, 10.0, 5.0)];
		let moves = KnobAssignment::of(&old).moves(&knobs(&[0.9]), &new);
		assert_eq!(moves, vec![(0, 0.5)]);
	}

	#[test]
	fn unknown_assignment_adopts_without_moving() {
		let params = [param("cutoff", 0.0, 10.0, 5.0)];
		assert_eq!(KnobAssignment::Unknown.moves(&knobs(&[0.1]), &params), vec![]);
	}

	#[test]
	fn parameters_after_the_last_knob_get_none() {
		let params: Vec<_> = (0..NUM_KNOBS + 2).map(|i| param(&format!("p{i}"), 0.0, 1.0, 1.0)).collect();
		let moves = KnobAssignment::default().moves(&knobs(&[]), &params);
		assert_eq!(moves.len(), NUM_KNOBS);
		assert_eq!(KnobAssignment::of(&params), KnobAssignment::Known(
			params[..NUM_KNOBS].iter().map(|p| Some(KnobParameter::from(p))).collect()));
	}

	#[test]
	fn defaults_outside_the_range_and_empty_ranges_are_clamped() {
		let params = [param("over", 0.0, 10.0, 20.0), param("flat", 3.0, 3.0, 3.0), param("down", 10.0, 0.0, 2.5)];
		let moves = KnobAssignment::default().moves(&knobs(&[0.5, 0.5, 0.5]), &params);
		assert_eq!(moves, vec![(0, 1.0), (1, 0.0), (2, 0.75)]);
	}
}
