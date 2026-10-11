//! The plugin's window: the MIDI mappings of the program, a slider for each
//! parameter, and a status bar.

use std::sync::Arc;

use nice_plug::context::gui::GuiContext;
use nice_plug::prelude::*;
use nice_plug_egui::baseview::HandlerError;
use nice_plug_egui::{Frame, NiceEguiApp};

use crate::JinglerParams;
use crate::knobs::{KnobAssignment, NUM_KNOBS, format_value, normalize};
use crate::mappings::mapping_rows;
use crate::status::shared;

pub struct JinglerEditor {
	params: Arc<JinglerParams>,
	/// Set while the window is open.
	context: Option<GuiContext>,
	/// The knob whose slider is being dragged.
	dragging: Option<usize>,
}

impl JinglerEditor {
	pub fn new(params: Arc<JinglerParams>) -> Self {
		JinglerEditor { params, context: None, dragging: None }
	}

	/// Move the knobs to the program's parameters (ADR 0006). Knobs can only
	/// be moved in a way the DAW sees from the editor, so this happens when
	/// the window is open.
	fn assign_knobs(&self, setter: &ParamSetter, parameters: &[ir::Parameter]) {
		let target = KnobAssignment::of(parameters);
		let mut assignment = self.params.knob_assignment.lock().unwrap();
		if *assignment == target {
			return;
		}
		let values: Vec<f32> = (0..NUM_KNOBS).map(|knob| self.params.knob(knob).unmodulated_normalized_value()).collect();
		for (knob, value) in assignment.moves(&values, parameters) {
			let param = self.params.knob(knob);
			setter.begin_set_parameter(param);
			setter.set_parameter_normalized(param, value);
			setter.end_set_parameter(param);
		}
		*assignment = target;
	}

	fn parameters_ui(&mut self, ui: &mut egui::Ui, setter: &ParamSetter, parameters: &[ir::Parameter]) {
		if parameters.is_empty() {
			ui.weak("The program has no parameters.");
			return;
		}
		let slider_width = (ui.available_width() * 0.5).max(120.0);
		egui::Grid::new("parameters").num_columns(3).spacing([12.0, 6.0]).show(ui, |ui| {
			for (index, parameter) in parameters.iter().enumerate() {
				if index < NUM_KNOBS {
					ui.label(&parameter.name);
					self.slider(ui, setter, index, parameter, slider_width);
				} else {
					ui.add_enabled(false, egui::Label::new(&parameter.name));
					ui.add_enabled(false, egui::Label::new("no knob"));
					ui.add_enabled(false, egui::Label::new(format_value(parameter.default)));
				}
				ui.end_row();
			}
		});
	}

	fn slider(&mut self, ui: &mut egui::Ui, setter: &ParamSetter, knob: usize, parameter: &ir::Parameter, width: f32) {
		let param = self.params.knob(knob);
		let ir::Parameter { min, max, default, .. } = *parameter;
		let mut value = min + param.unmodulated_normalized_value() * (max - min);

		ui.spacing_mut().slider_width = width;
		let response = ui.add_enabled(min != max, egui::Slider::new(&mut value, min..=max).show_value(false));
		let double_clicked = response.hovered()
			&& ui.input(|input| input.pointer.button_double_clicked(egui::PointerButton::Primary));
		if double_clicked {
			value = default;
		}

		// A drag is one gesture for the DAW; any other change is a gesture of its own.
		if response.drag_started() {
			setter.begin_set_parameter(param);
			self.dragging = Some(knob);
		}
		let dragging = self.dragging == Some(knob);
		if response.changed() || double_clicked {
			if !dragging {
				setter.begin_set_parameter(param);
			}
			setter.set_parameter_normalized(param, normalize(value, min, max));
			if !dragging {
				setter.end_set_parameter(param);
			}
		}
		if response.drag_stopped() && dragging {
			setter.end_set_parameter(param);
			self.dragging = None;
		}
		response.on_hover_text(format!(
			"{} to {}, default {} (double-click)",
			format_value(min), format_value(max), format_value(default)));

		ui.monospace(format_value(value));
	}
}

fn mappings_ui(ui: &mut egui::Ui, program: &ir::Program) {
	let rows = mapping_rows(program);
	if rows.is_empty() {
		ui.weak("The program plays no instruments.");
		return;
	}
	egui::Grid::new("mappings").num_columns(3).spacing([12.0, 6.0]).striped(true).show(ui, |ui| {
		for row in rows {
			ui.label(format!("Ch {}", row.channel()));
			match row.keys() {
				Some(keys) => ui.monospace(keys).on_hover_text(row.key_numbers()),
				None => ui.weak("all keys"),
			};
			ui.label(&row.instrument);
			ui.end_row();
		}
	});
}

impl NiceEguiApp for JinglerEditor {
	fn build(&mut self, _egui_ctx: egui::Context, context: GuiContext, _frame: &mut Frame) -> Result<(), HandlerError> {
		self.context = Some(context);
		Ok(())
	}

	fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut Frame) {
		self.draw(ui);
	}

	fn editor_closed(&mut self) {
		if let (Some(knob), Some(context)) = (self.dragging.take(), &self.context) {
			context.param_setter().end_set_parameter(self.params.knob(knob));
		}
		self.context = None;
	}
}

impl JinglerEditor {
	fn draw(&mut self, ui: &mut egui::Ui) {
		let Some(context) = self.context.clone() else { return };
		let setter = context.param_setter();
		let (program, received, error) = {
			let status = shared().status();
			(status.program.clone(), status.received.clone(), status.error.clone())
		};
		if let Some(program) = &program && self.dragging.is_none() {
			self.assign_knobs(&setter, &program.parameters);
		}

		egui::Panel::bottom("status").show(ui, |ui| {
			ui.add_space(4.0);
			match &received {
				Some(time) => ui.label(format!("Program received {time}")),
				None => ui.label("No program received. Run zing-cmd --connect to send one."),
			};
			if let Some(error) = &error {
				ui.colored_label(ui.visuals().error_fg_color, error);
			}
			ui.add_space(4.0);
		});
		let Some(program) = &program else {
			egui::CentralPanel::default().show(ui, |_| {});
			return;
		};
		egui::Panel::left("mappings").resizable(true).default_size(280.0).min_size(160.0).show(ui, |ui| {
			ui.heading("MIDI mappings");
			egui::ScrollArea::vertical().id_salt("mappings").show(ui, |ui| mappings_ui(ui, program));
		});
		egui::CentralPanel::default().show(ui, |ui| {
			ui.heading("Parameters");
			egui::ScrollArea::vertical().id_salt("parameters").show(ui, |ui| {
				self.parameters_ui(ui, &setter, &program.parameters);
			});
		});
	}
}

#[cfg(test)]
mod tests {
	use std::sync::Mutex;

	use super::*;

	/// A DAW that records the knob edits it is told about.
	struct Host {
		params: Arc<JinglerParams>,
		edits: Mutex<Vec<String>>,
	}

	impl Host {
		fn record(&self, edit: &str, param: ParamPtr) {
			let knob = (0..NUM_KNOBS).position(|k| self.params.knob(k).as_ptr() == param).unwrap();
			self.edits.lock().unwrap().push(format!("{edit} {knob}"));
		}
	}

	impl GuiContextInner for Host {
		fn plugin_api(&self) -> PluginApi { PluginApi::Vst3 }
		unsafe fn raw_begin_set_parameter(&self, param: ParamPtr) { self.record("begin", param) }
		unsafe fn raw_set_parameter_normalized(&self, param: ParamPtr, normalized: f32) {
			self.record(&format!("set {normalized}"), param)
		}
		unsafe fn raw_end_set_parameter(&self, param: ParamPtr) { self.record("end", param) }
		fn get_state(&self) -> PluginState { unimplemented!() }
		fn set_state(&self, _state: PluginState) {}
		fn request_restart(&self) {}
	}

	fn program() -> ir::Program {
		let procedure = |name: &str, kind, code| ir::Procedure {
			name: name.to_string(), kind, inputs: vec![], outputs: vec![], code,
		};
		let parameter = |name: &str, min, max, default| ir::Parameter { name: name.to_string(), min, max, default };
		ir::Program {
			parameters: (0..NUM_KNOBS + 1).map(|i| match i {
				0 => parameter("cutoff", 100.0, 4100.0, 900.0),
				_ => parameter(&format!("p{i}"), 0.0, 1.0, 0.5),
			}).collect(),
			procedures: vec![
				procedure("main", ir::ProcedureKind::Module { scope: ir::Scope::Dynamic }, vec![
					ir::Instruction::PlayInstrument(1, 2),
				]),
				procedure("bass", ir::ProcedureKind::Instrument { scope: ir::Scope::Static }, vec![]),
				procedure("bass", ir::ProcedureKind::Instrument { scope: ir::Scope::Dynamic }, vec![]),
			],
			externals: vec![],
			main_static_proc_id: 0,
			main_dynamic_proc_id: 0,
			track_order: vec![ir::MidiMapping { channel: 0, start: 48, end: 72, transpose_to: 36 }],
		}
	}

	#[test]
	fn the_open_editor_moves_new_parameters_to_their_defaults() {
		shared().accept_program(program());
		let params = Arc::new(JinglerParams::default());
		let host = Arc::new(Host { params: params.clone(), edits: Mutex::new(vec![]) });
		let mut editor = JinglerEditor::new(params.clone());
		editor.context = Some(GuiContext::new(host.clone()));

		let ctx = egui::Context::default();
		for _ in 0..2 {
			ctx.run_ui(egui::RawInput::default(), |ui| editor.draw(ui)).textures_delta.clear();
		}

		// Only cutoff is away from its default, and only once.
		assert_eq!(*host.edits.lock().unwrap(), vec!["begin 0", "set 0.2 0", "end 0"]);
		assert_eq!(*params.knob_assignment.lock().unwrap(), KnobAssignment::of(&program().parameters));
	}
}
