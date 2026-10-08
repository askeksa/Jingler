
use anyhow::{anyhow, Result};
use strum_macros::FromRepr;

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

use crate::program::{ExternalProcedure, ProcedureKind, Program, Scope, Width};
use crate::instructions::{Instruction, NoteProperty};


const RANDOM_SCRAMBLE: u32 = 0x42118159;

#[allow(unused)]
#[derive(Clone, Copy, Debug, Eq, FromRepr, PartialEq)]
enum EncodedBytecode {
	StateEnter,
	StateLeave,
	CellFetch,
	CellPush,
	CellRead,
	CellInit,
	CellPop,
	GmDlsSample,
	AddSub,
	Fputnext,
	Random,
	BufferLoadWithOffset,
	BufferLoadIndexed,
	BufferLoad,
	BufferStoreAndStep,
	BufferAlloc,
	PlayInstrument1,
	PlayInstrument2,
	PlayInstrument3,
	Kill,
	Exp2Body,
	Fdone,
	Fdone2,
	Fld1,
	Fop,
	Proc,
	ProcCall,
	ReadNoteProperty,
	Constant,
	ConstantByteIndex,
	ProcCallByteIndex,
	StackLoad,
	StackStore,
	Label,
	If,
	LoopJump,
	EndIf,
	Else,
	Round,
	Compare,
	Implicit,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncodedFop {
	Fyl2x = 0xF1,
	Fptan = 0xF2,
	Fpatan = 0xF3,
	Fsincos = 0xFB,
	Frndint = 0xFC,
	Fsin = 0xFE,
	Fcos = 0xFF,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncodedNoteProperty {
	Length = 0,
	Key = 1,
	Velocity = 2,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EncodedRoundingMode {
	Nearest = 0,
	Floor = 1,
	Ceil = 2,
	Truncate = 3,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EncodedCompareOp {
	Eq = 0,
	Less = 1,
	LessEq = 2,
	Neq = 4,
	GreaterEq = 5,
	Greater = 6,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncodedImplicit {
	MergeLR = 0x14,
	SplitRL = 0x15,
	ExpandL = 0x16,
	ExpandR = 0x17,
	Pop = 0x28,
	PopNext = 0x29,
	BufferIndexAndLength = 0x2A,
	Cmp = 0x2E,
	Sqrt = 0x51,
	And = 0x54,
	AndNot = 0x55,
	Or = 0x56,
	Xor = 0x57,
	Add = 0x58,
	Mul = 0x59,
	Sub = 0x5C,
	Min = 0x5D,
	Div = 0x5E,
	Max = 0x5F,
}

const fn implicit_encoding(implicit: EncodedImplicit) -> u16 {
	let opcode = implicit as u16;
	opcode - (1 << (15 - opcode.leading_zeros())) - 4
}

fn bytecode_name(opcode: EncodedBytecode, arg: u16) -> (&'static str, Option<u16>) {
	use EncodedBytecode::*;
	match opcode {
		StateEnter => ("state_enter", None),
		StateLeave => ("state_leave", None),
		CellFetch => ("cell_fetch", None),
		CellPush => ("cell_push", None),
		CellRead => ("cell_read", None),
		CellInit => ("cell_init", None),
		CellPop => ("cell_pop", None),
		GmDlsSample => ("gmdls_sample", None),
		AddSub => ("addsub", None),
		Fputnext => ("fputnext", None),
		Random => ("random", None),
		BufferLoadWithOffset => ("buffer_load_with_offset", None),
		BufferLoadIndexed => ("buffer_load_indexed", None),
		BufferLoad => ("buffer_load", None),
		BufferStoreAndStep => ("buffer_store_and_step", None),
		BufferAlloc => ("buffer_alloc", None),
		PlayInstrument1 => ("play_instrument1", None),
		PlayInstrument2 => ("play_instrument2", None),
		PlayInstrument3 => ("play_instrument3", None),
		Kill => ("kill", None),
		Exp2Body => ("exp2_body", None),
		Fdone => ("fdone", None),
		Fdone2 => ("fdone2", None),
		Fld1 => ("fld1", None),
		Fop => ("fop", Some(arg)),
		Proc => ("proc", None),
		ProcCall => ("proc_call", Some(arg)),
		ReadNoteProperty => ("note_property", Some(arg)),
		Constant => ("constant", Some(arg)),
		ConstantByteIndex => ("constant_byte_index", Some(arg)),
		ProcCallByteIndex => ("proc_call_byte_index", Some(arg)),
		StackLoad => ("stack_load", Some(arg)),
		StackStore => ("stack_store", Some(arg)),
		Label => ("label", None),
		If => ("if", None),
		LoopJump => ("loopjump", None),
		EndIf => ("endif", None),
		Else => ("else", None),
		Round => ("round", Some(arg)),
		Compare => ("compare", Some(arg)),
		Implicit => (implicit_name(arg), None),
	}
}

fn implicit_name(encoding: u16) -> &'static str {
	use EncodedImplicit::*;
	match encoding {
		e if e == implicit_encoding(MergeLR) => "merge_lr",
		e if e == implicit_encoding(SplitRL) => "split_rl",
		e if e == implicit_encoding(ExpandL) => "expand_l",
		e if e == implicit_encoding(ExpandR) => "expand_r",
		e if e == implicit_encoding(Pop) => "pop",
		e if e == implicit_encoding(PopNext) => "popnext",
		e if e == implicit_encoding(BufferIndexAndLength) => "buffer_index_and_length",
		e if e == implicit_encoding(Cmp) => "cmp",
		e if e == implicit_encoding(Sqrt) => "sqrt",
		e if e == implicit_encoding(And) => "and",
		e if e == implicit_encoding(AndNot) => "andn",
		e if e == implicit_encoding(Or) => "or",
		e if e == implicit_encoding(Xor) => "xor",
		e if e == implicit_encoding(Add) => "add",
		e if e == implicit_encoding(Mul) => "mul",
		e if e == implicit_encoding(Sub) => "sub",
		e if e == implicit_encoding(Min) => "min",
		e if e == implicit_encoding(Div) => "div",
		e if e == implicit_encoding(Max) => "max",
		_ => panic!("unknown implicit encoding"),
	}
}

fn encode_fop(fop: EncodedFop, encode: &mut impl FnMut(EncodedBytecode, u16)) {
	let encoding = 0xFF - fop as u16;
	encode(EncodedBytecode::Fop, encoding);
}

fn encode_note_property(property: EncodedNoteProperty, encode: &mut impl FnMut(EncodedBytecode, u16)) {
	encode(EncodedBytecode::ReadNoteProperty, property as u16);
}

fn encode_rounding(mode: EncodedRoundingMode, encode: &mut impl FnMut(EncodedBytecode, u16)) {
	encode(EncodedBytecode::Round, mode as u16);
}

fn encode_comparison(op: EncodedCompareOp, encode: &mut impl FnMut(EncodedBytecode, u16)) {
	encode(EncodedBytecode::Compare, op as u16);
}

fn encode_implicit(implicit: EncodedImplicit, encode: &mut impl FnMut(EncodedBytecode, u16)) {
	encode(EncodedBytecode::Implicit, implicit_encoding(implicit));
}

fn encode_bytecode(inst: Instruction, sample_rate: f32,
                   encode: &mut impl FnMut(EncodedBytecode, u16),
                   encode_constant: &mut impl FnMut(u32),
				   encode_parameter: &mut impl FnMut(u16),
				   encode_external: &mut impl FnMut(u16)) {
	use EncodedBytecode::*;
	use EncodedFop::*;
	use EncodedNoteProperty::*;
	use EncodedRoundingMode::*;
	use EncodedCompareOp::*;
	use EncodedImplicit::*;
	match inst {
		Instruction::StateEnter => encode(StateEnter, 0),
		Instruction::StateLeave => encode(StateLeave, 0),
		Instruction::CellFetch => encode(CellFetch, 0),
		Instruction::CellPush(..) => encode(CellPush, 0),
		Instruction::CellRead(..) => encode(CellRead, 0),
		Instruction::CellInit(..) => encode(CellInit, 0),
		Instruction::CellPop => encode(CellPop, 0),
		Instruction::GmDlsSample => encode(GmDlsSample, 0),
		Instruction::AddSub => encode(AddSub, 0),
		Instruction::Random => encode(Random, 0),
		Instruction::BufferLoadWithOffset => encode(BufferLoadWithOffset, 0),
		Instruction::BufferLoadIndexed => encode(BufferLoadIndexed, 0),
		Instruction::BufferLoad => encode(BufferLoad, 0),
		Instruction::BufferStoreAndStep => encode(BufferStoreAndStep, 0),
		Instruction::BufferAlloc(..) => encode(BufferAlloc, 0),
		Instruction::Kill => encode(Kill, 0),
		Instruction::Call(proc, ..) => encode(ProcCall, proc),
		Instruction::CallExternal(index) => encode_external(index),
		Instruction::Constant(constant) => encode_constant(constant),
		Instruction::SampleRate => encode_constant(sample_rate.to_bits()),
		Instruction::Parameter(index) => encode_parameter(index),
		Instruction::StackLoad(offset) => encode(StackLoad, offset),
		Instruction::StackStore(offset) => encode(StackStore, offset),

		Instruction::PlayInstrument(static_proc, dynamic_proc) => {
			encode(PlayInstrument1, 0);
			encode(ProcCall, static_proc);
			encode(PlayInstrument2, 0);
			encode(ProcCall, dynamic_proc);
			encode(PlayInstrument3, 0);
		},

		Instruction::RepeatInit => {
			encode(StackLoad, 0);
			encode(CellInit, 0);
			encode_constant(0);
			encode(Label, 0);
		},
		Instruction::RepeatStart => {
			encode(CellRead, 0);
			encode_constant(0);
			encode(Label, 0);
		},
		Instruction::RepeatEnd => {
			encode_constant(0x3F800000);
			encode_implicit(Add, encode);
			encode_implicit(Cmp, encode);
			encode(LoopJump, 0);
			encode_implicit(Pop, encode);
			encode_implicit(Pop, encode);
		},

		Instruction::BufferInitStart => {
			encode(Label, 0);
		},
		Instruction::BufferInitEnd => {
			encode(BufferStoreAndStep, 0);
			encode(LoopJump, 0);
		},

		Instruction::IfGreaterEq => {
			encode_implicit(Cmp, encode);
			encode(If, 0);
		},
		Instruction::Else => {
			encode(Else, 0);
			encode(Label, 0);
		},
		Instruction::EndIf => {
			encode(EndIf, 0);
		},

		Instruction::Ceil => encode_rounding(Ceil, encode),
		Instruction::Floor => encode_rounding(Floor, encode),
		Instruction::Round => encode_rounding(Nearest, encode),
		Instruction::Trunc => encode_rounding(Truncate, encode),

		Instruction::Eq => encode_comparison(Eq, encode),
		Instruction::Greater => encode_comparison(Greater, encode),
		Instruction::GreaterEq => encode_comparison(GreaterEq, encode),
		Instruction::Less => encode_comparison(Less, encode),
		Instruction::LessEq => encode_comparison(LessEq, encode),
		Instruction::Neq => encode_comparison(Neq, encode),

		Instruction::Atan2 => {
			encode(Fputnext, 0);
			encode_fop(Fpatan, encode);
			encode(Fdone, 0);
		},
		Instruction::Cos => {
			encode_fop(Fcos, encode);
			encode(Fdone, 0);
		},
		Instruction::Exp2 => {
			encode_fop(Frndint, encode);
			encode(Exp2Body, 0);
			encode(Fdone, 0);
		},
		Instruction::Log2 => {
			encode(Fld1, 0);
			encode_fop(Fyl2x, encode);
			encode(Fdone, 0);
		},
		Instruction::Pow => {
			encode(Fputnext, 0);
			encode_fop(Fyl2x, encode);
			encode(Fdone, 0);
			encode_fop(Frndint, encode);
			encode(Exp2Body, 0);
			encode(Fdone, 0);
		},
		Instruction::Sin => {
			encode_fop(Fsin, encode);
			encode(Fdone, 0);
		},
		Instruction::SinCos => {
			encode_fop(Fsincos, encode);
			encode(Fdone, 0);
			encode(Fdone2, 0);
		},
		Instruction::Tan => {
			encode_fop(Fptan, encode);
			encode(Fdone, 0);
			encode(Fdone, 0);
		},

		Instruction::ReadNoteProperty(NoteProperty::Gate) => {
			encode_constant(0);
			encode_note_property(Length, encode);
			encode_comparison(Greater, encode);
		},
		Instruction::ReadNoteProperty(NoteProperty::Key) => encode_note_property(Key, encode),
		Instruction::ReadNoteProperty(NoteProperty::Velocity) => encode_note_property(Velocity, encode),

		Instruction::MergeLR => encode_implicit(MergeLR, encode),
		Instruction::SplitRL => encode_implicit(SplitRL, encode),
		Instruction::Left => {},
		Instruction::Right => encode_implicit(ExpandR, encode),
		Instruction::Expand(Width::Mono) => {},
		Instruction::Expand(_) => encode_implicit(ExpandL, encode),
		Instruction::Pop => encode_implicit(Pop, encode),
		Instruction::PopNext => encode_implicit(PopNext, encode),
		Instruction::BufferIndex => encode_implicit(BufferIndexAndLength, encode),
		Instruction::BufferLength => {
			encode_implicit(BufferIndexAndLength, encode);
			encode_implicit(ExpandR, encode);
		},
		Instruction::Sqrt => encode_implicit(Sqrt, encode),
		Instruction::And => encode_implicit(And, encode),
		Instruction::AndNot => encode_implicit(AndNot, encode),
		Instruction::Or => encode_implicit(Or, encode),
		Instruction::Xor => encode_implicit(Xor, encode),
		Instruction::Add => encode_implicit(Add, encode),
		Instruction::Mul => encode_implicit(Mul, encode),
		Instruction::Sub => encode_implicit(Sub, encode),
		Instruction::Min => encode_implicit(Min, encode),
		Instruction::Div => encode_implicit(Div, encode),
		Instruction::Max => encode_implicit(Max, encode),
	}
}

fn collect_capacities(program: &Program, sample_rate: f32, embed_constant_index: bool) -> (Vec<u16>, BTreeSet<u32>, Vec<u16>) {
	// Collect arg space for each opcode
	let mut opcode_capacity = vec![0u16; EncodedBytecode::Implicit as usize + 1];
	let mut discover_opcode = |opcode: EncodedBytecode, arg: u16| {
		let capacity = &mut opcode_capacity[opcode as usize];
		*capacity = (*capacity).max(arg + 1);
	};
	let mut constant_set = BTreeSet::new();
	let mut discover_constant = |value: u32| {
		constant_set.insert(value);
	};
	let mut discover_parameter = |_index: u16| {};
	let mut external_capacity = vec![0u16; program.externals.len()];
	let mut discover_external = |index: u16| {
		external_capacity[index as usize] = 1;
	};
	for &inst in program.procedures.iter().map(|p| &p.code).flatten() {
		encode_bytecode(inst, sample_rate, &mut discover_opcode, &mut discover_constant, &mut discover_parameter, &mut discover_external);
	}
	if opcode_capacity[EncodedBytecode::Random as usize] != 0 {
		constant_set.insert(RANDOM_SCRAMBLE);
	}
	opcode_capacity[EncodedBytecode::Proc as usize] = 1;
	if embed_constant_index {
		opcode_capacity[EncodedBytecode::Constant as usize] = (program.parameters.len() + constant_set.len() + 1) as u16;
	} else {
		opcode_capacity[EncodedBytecode::ConstantByteIndex as usize] = 1;
	}

	(opcode_capacity, constant_set, external_capacity)
}

fn build_constant_list(program: &Program, constant_set: &BTreeSet<u32>) -> (Vec<u32>, BTreeMap<u32, u16>, usize) {
	let mut constants: Vec<u32> = vec![];
	if constant_set.contains(&RANDOM_SCRAMBLE) {
		constants.push(RANDOM_SCRAMBLE);
	}
	let parameter_offset = constants.len();
	constants.extend(vec![0; program.parameters.len()]);
	constants.extend(constant_set.into_iter().filter(|&&v| v != RANDOM_SCRAMBLE));
	let mut constant_map = BTreeMap::new();
	for (i, &v) in constants.iter().enumerate() {
		constant_map.insert(v, i as u16);
	}

	(constants, constant_map, parameter_offset)
}

fn check_opcode_space(opcode_capacity: &Vec<u16>, external_capacity: &Vec<u16>, constants: &Vec<u32>) -> Result<()> {
	let opcode_space = opcode_capacity.iter().sum::<u16>() + external_capacity.iter().sum::<u16>() + 1;
	if opcode_space > 256 {
		return Err(anyhow!("\nExceeded opcode space ({} > {}).", opcode_space, 256));
	}
	let constant_space = constants.len();
	if constant_space > 256 {
		return Err(anyhow!("\nExceeded constant space ({} > {}).", constant_space, 256));
	}
	Ok(())
}

// The player implements each external procedure as a hand-written player
// instruction named after it: external_function_<name> for a function,
// external_module_<name>_dynamic and external_module_<name>_static for the
// parts of a module. These names are distinct for distinct procedures.
fn external_instruction_name(external: &ExternalProcedure) -> Result<String> {
	match external.kind {
		ProcedureKind::Function => Ok(format!("external_function_{}", external.name)),
		ProcedureKind::Module { scope: Scope::Dynamic } => Ok(format!("external_module_{}_dynamic", external.name)),
		ProcedureKind::Module { scope: Scope::Static } => Ok(format!("external_module_{}_static", external.name)),
		ProcedureKind::Instrument { .. } => Err(anyhow!("\nInstrument '{}' can't be external.", external.name)),
	}
}

// Name the player instruction of each external procedure, rejecting names
// that differ only in case, since their I_ defines would collide.
fn external_instruction_names(program: &Program) -> Result<Vec<String>> {
	let names = program.externals.iter()
		.map(external_instruction_name)
		.collect::<Result<Vec<String>>>()?;
	for (i, name) in names.iter().enumerate() {
		for (j, other) in names[..i].iter().enumerate() {
			let (member, other_member) = (&program.externals[i].name, &program.externals[j].name);
			if name.to_uppercase() == other.to_uppercase() {
				return Err(anyhow!("\nExternal members '{}' and '{}' both need the define 'I_{}'.",
					other_member, member, name.to_uppercase()));
			}
		}
	}
	Ok(names)
}

pub fn encode_bytecodes_source(
		program: &Program, jingler_asm_path: &String,
		sample_rate: f32, embed_constant_index: bool, parameter_quantization: f32,
		out: &mut impl std::io::Write) -> Result<()> {
	let external_names = external_instruction_names(program)?;
	let (opcode_capacity, constant_set, external_capacity) = collect_capacities(program, sample_rate, embed_constant_index);
	let (constants, constant_map, parameter_offset) = build_constant_list(program, &constant_set);
	check_opcode_space(&opcode_capacity, &external_capacity, &constants)?;

	for ((name, external), capacity) in external_names.iter().zip(&program.externals).zip(&external_capacity) {
		writeln!(out, "%define I_{} {} ; {}", name.to_uppercase(), capacity, external)?;
	}
	for i in 0 .. EncodedBytecode::Implicit as usize {
		let (name, _) = bytecode_name(EncodedBytecode::from_repr(i).unwrap(), 0);
		writeln!(out, "%define I_{} {}", name.to_uppercase(), opcode_capacity[i])?;
	}
	writeln!(out, "\n%define COMPACT_IMPLICIT_OPCODES 1")?;

	writeln!(out, "\n%define MAIN_STATIC_PROC_ID {}", program.main_static_proc_id)?;
	writeln!(out, "%define MAIN_DYNAMIC_PROC_ID {}", program.main_dynamic_proc_id)?;

	writeln!(out, "\n%define NUM_TRACKS {}", program.track_order.len())?;
	writeln!(out, "\n%define NUM_PARAMETERS {}", program.parameters.len())?;
	writeln!(out, "\n%define PARAMETER_OFFSET {}", parameter_offset)?;

	writeln!(out, "\n%include \"{}\"", jingler_asm_path)?;

	writeln!(out, "\nsection musdat data align=1")?;
	writeln!(out, "\n%define b(n) _snip_id_%+n")?;
	if embed_constant_index {
		writeln!(out, "%define c(n) _snip_id_constant+n")?;
	} else {
		writeln!(out, "%define c(n) _snip_id_constant_byte_index,n")?;
	}

	writeln!(out, "\nBytecodes:")?;
	writeln!(out, "\tdb\tb(proc)")?;
	for procedure in &program.procedures {
		match procedure.kind {
			ProcedureKind::Function => writeln!(out, ".function_{}:", procedure.name),
			ProcedureKind::Module { scope } => writeln!(out, ".module_{}_{}:", procedure.name, scope),
			ProcedureKind::Instrument { scope } => writeln!(out, ".instrument_{}_{}:", procedure.name, scope),
		}?;
		let mut first = true;
		for &inst in &procedure.code {
			let codes = RefCell::new(vec![]);
			let mut encode_opcode = |opcode: EncodedBytecode, arg: u16| {
				let (name, offset) = bytecode_name(opcode, arg);
				let s = if let Some(offset) = offset {
					format!("b({name})+{offset}")
				} else {
					format!("b({name})")
				};
				codes.borrow_mut().push(s);
			};
			let mut encode_constant = |value: u32| {
				let arg = constant_map[&value];
				let s = format!("c({arg})");
				codes.borrow_mut().push(s);
			};
			let mut encode_parameter = |index: u16| {
				let arg = parameter_offset as u16 + index;
				let s = format!("c({arg})");
				codes.borrow_mut().push(s);
			};
			let mut encode_external = |index: u16| {
				let s = format!("b({})", external_names[index as usize]);
				codes.borrow_mut().push(s);
			};
			encode_bytecode(inst, sample_rate, &mut encode_opcode, &mut encode_constant, &mut encode_parameter, &mut encode_external);
			for code in codes.borrow().iter() {
				if first {
					write!(out, "\tdb\t{code}")?;
					first = false;
				} else {
					write!(out, ",{code}")?;
				}
			}
		}
		writeln!(out, "\n\tdb\tb(proc)")?;
	}
	writeln!(out, "\tdb\t0")?;

	write!(out, "\nConstantPool:")?;
	for (i, &value) in constants.iter().enumerate() {
		if i % 10 == 0 {
			write!(out, "\n\tdd\t0x{:08X}", value)?;
		} else {
			write!(out, ", 0x{:08X}", value)?;
		}
	}
	writeln!(out)?;

	writeln!(out, "\nsection paramsb rdata align=4")?;

	writeln!(out, "\nParameterScaleBias:")?;
	for param in &program.parameters {
		let scale = (param.max - param.min) * parameter_quantization;
		let bias = param.min;
		writeln!(out, "\tdd\t0x{:08X}, 0x{:08X}", scale.to_bits(), bias.to_bits())?;
	}

	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::program::{Procedure, Type, ValueType};

	const MONO: Type = Type { width: Width::Mono, value_type: ValueType::Number };
	const STEREO: Type = Type { width: Width::Stereo, value_type: ValueType::Number };
	const STATIC: ProcedureKind = ProcedureKind::Module { scope: Scope::Static };
	const DYNAMIC: ProcedureKind = ProcedureKind::Module { scope: Scope::Dynamic };

	fn external(name: &str, kind: ProcedureKind, inputs: Vec<Type>, outputs: Vec<Type>) -> ExternalProcedure {
		ExternalProcedure { name: name.to_string(), kind, inputs, outputs }
	}

	fn program(externals: Vec<ExternalProcedure>, static_code: Vec<Instruction>, dynamic_code: Vec<Instruction>) -> Program {
		let main = |scope, code| Procedure {
			name: "main".to_string(), kind: ProcedureKind::Module { scope }, inputs: vec![], outputs: vec![STEREO], code,
		};
		Program {
			parameters: vec![],
			procedures: vec![main(Scope::Static, static_code), main(Scope::Dynamic, dynamic_code)],
			externals,
			main_static_proc_id: 0,
			main_dynamic_proc_id: 1,
			track_order: vec![],
		}
	}

	fn encode(program: &Program) -> Result<String> {
		let mut out = vec![];
		encode_bytecodes_source(program, &"jingler.asm".to_string(), 44100.0, false, 16.0, &mut out)?;
		Ok(String::from_utf8(out).unwrap())
	}

	#[test]
	fn external_members_encode_as_named_player_instructions() {
		use Instruction::*;
		let program = program(
			vec![
				external("osc", DYNAMIC, vec![MONO], vec![STEREO]),
				external("osc", STATIC, vec![MONO, STEREO], vec![]),
				external("pan", ProcedureKind::Function, vec![MONO, MONO], vec![STEREO]),
			],
			vec![Constant(0), Constant(0), CallExternal(1)],
			vec![Constant(0), CallExternal(0), Constant(0), Constant(0), CallExternal(2), Add],
		);
		let source = encode(&program).unwrap();
		assert!(source.contains("%define I_EXTERNAL_MODULE_OSC_DYNAMIC 1 ; osc [external module, dynamic part]: (mono number) -> (stereo number)\n"), "{source}");
		assert!(source.contains("%define I_EXTERNAL_MODULE_OSC_STATIC 1 ; osc [external module, static part]: (mono number, stereo number) -> ()\n"), "{source}");
		assert!(source.contains("%define I_EXTERNAL_FUNCTION_PAN 1 ; pan [external function]: (mono number, mono number) -> (stereo number)\n"), "{source}");
		assert!(source.contains("\tdb\tc(0),c(0),b(external_module_osc_static)\n"), "{source}");
		assert!(source.contains("\tdb\tc(0),b(external_module_osc_dynamic),c(0),c(0),b(external_function_pan),b(add)\n"), "{source}");
	}

	#[test]
	fn uncalled_external_parts_get_no_opcode() {
		use Instruction::*;
		let program = program(
			vec![external("pan", ProcedureKind::Function, vec![MONO], vec![MONO])],
			vec![],
			vec![Constant(0)],
		);
		let source = encode(&program).unwrap();
		assert!(source.contains("%define I_EXTERNAL_FUNCTION_PAN 0 ;"), "{source}");
	}

	#[test]
	fn functions_and_module_parts_get_distinct_player_instructions() {
		let program = program(
			vec![
				external("osc", DYNAMIC, vec![], vec![MONO]),
				external("osc", STATIC, vec![], vec![]),
				external("osc_static", DYNAMIC, vec![], vec![MONO]),
				external("osc_static", STATIC, vec![], vec![]),
				external("osc", ProcedureKind::Function, vec![], vec![MONO]),
				external("osc_static", ProcedureKind::Function, vec![], vec![MONO]),
			],
			vec![],
			vec![],
		);
		let source = encode(&program).unwrap();
		for define in [
			"I_EXTERNAL_MODULE_OSC_DYNAMIC", "I_EXTERNAL_MODULE_OSC_STATIC",
			"I_EXTERNAL_MODULE_OSC_STATIC_DYNAMIC", "I_EXTERNAL_MODULE_OSC_STATIC_STATIC",
			"I_EXTERNAL_FUNCTION_OSC", "I_EXTERNAL_FUNCTION_OSC_STATIC",
		] {
			assert!(source.contains(&format!("%define {define} 0 ;")), "{source}");
		}
	}

	#[test]
	fn colliding_defines_are_rejected() {
		let program = program(
			vec![
				external("Pan", ProcedureKind::Function, vec![], vec![MONO]),
				external("pan", ProcedureKind::Function, vec![], vec![MONO]),
			],
			vec![],
			vec![],
		);
		let error = encode(&program).unwrap_err().to_string();
		assert!(error.contains("External members 'Pan' and 'pan' both need the define 'I_EXTERNAL_FUNCTION_PAN'."), "{error}");
	}
}
