use crate::instructions::Instruction;
use crate::program::Program;

/// Result of comparing a newly-submitted program against a previously accepted one.
#[derive(Debug, Clone, PartialEq)]
pub enum Diff {
	/// Programs are byte-for-byte identical (including constant values).
	Identical,
	/// Only `Constant(u32)` values differed. Vec entries are `(slot_index, new_value_bits)`,
	/// containing only the slots that actually changed.
	ConstantsOnly(Vec<(u32, u32)>),
	/// Anything else differed; a fresh recompile is required.
	Structural,
}

/// Walk `program.procedures` in index order; within each procedure, walk
/// `procedure.code` in order. Every `Instruction::Constant(v)` is assigned
/// the next sequential `slot: u32` starting at 0. The returned vector contains
/// the raw `u32` bit patterns indexed by slot.
///
/// This traversal is the single source of truth for constant slot indexing;
/// any consumer that addresses constants by slot (e.g. the runtime's Wasm
/// global encoding) must agree on it.
pub fn collect_constants(program: &Program) -> Vec<u32> {
	let mut out = Vec::new();
	for proc in &program.procedures {
		for instr in &proc.code {
			if let Instruction::Constant(v) = *instr {
				out.push(v);
			}
		}
	}
	out
}

/// Classify a new program relative to a previously accepted program.
pub fn diff_programs(old: &Program, new: &Program) -> Diff {
	if old.parameters != new.parameters
		|| old.main_static_proc_id != new.main_static_proc_id
		|| old.main_dynamic_proc_id != new.main_dynamic_proc_id
		|| old.track_order != new.track_order
		|| old.procedures.len() != new.procedures.len()
	{
		return Diff::Structural;
	}

	let mut changes = Vec::new();
	let mut slot: u32 = 0;
	for (op, np) in old.procedures.iter().zip(new.procedures.iter()) {
		if op.name != np.name
			|| op.kind != np.kind
			|| op.inputs != np.inputs
			|| op.outputs != np.outputs
			|| op.code.len() != np.code.len()
		{
			return Diff::Structural;
		}
		for (oi, ni) in op.code.iter().zip(np.code.iter()) {
			match (*oi, *ni) {
				(Instruction::Constant(ov), Instruction::Constant(nv)) => {
					if ov != nv {
						changes.push((slot, nv));
					}
					slot += 1;
				}
				(a, b) if a == b => {}
				_ => return Diff::Structural,
			}
		}
	}

	if changes.is_empty() {
		Diff::Identical
	} else {
		Diff::ConstantsOnly(changes)
	}
}
