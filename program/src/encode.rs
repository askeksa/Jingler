
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::mem::replace;

use crate::program::ZingProgram;
use crate::instructions::{Instruction, NoteProperty};


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncodedBytecode {
	StateEnter,
	StateLeave,
	Kill,
	CellInit,
	CellRead,
	GmDlsLength,
	GmDlsSample,
	AddSub,
	Fputnext,
	Random,
	BufferLoadWithOffset,
	BufferLoad,
	BufferStoreAndStep,
	BufferAlloc,
	CallInstrument,
	Exp2Body,
	Fdone,
	Fop,
	Proc,
	ProcCall,
	Constant,
	ReadNoteProperty,
	StackLoad,
	StackStore,
	CellStore,
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
	let opcode = implicit as u16;
	let encoding = opcode - (1 << (15 - opcode.leading_zeros())) - 4;
	encode(EncodedBytecode::Implicit, encoding);
}

fn encode_bytecode(inst: Instruction, sample_rate: f32,
                   encode: &mut impl FnMut(EncodedBytecode, u16),
                   encode_constant: &mut impl FnMut(u32)) {
	use EncodedBytecode::*;
	use EncodedFop::*;
	use EncodedNoteProperty::*;
	use EncodedRoundingMode::*;
	use EncodedCompareOp::*;
	use EncodedImplicit::*;
	match inst {
		Instruction::StateEnter => encode(StateEnter, 0),
		Instruction::StateLeave => encode(StateLeave, 0),
		Instruction::Kill => encode(Kill, 0),
		Instruction::CellInit => encode(CellInit, 0),
		Instruction::CellRead => encode(CellRead, 0),
		Instruction::GmDlsLength => encode(GmDlsLength, 0),
		Instruction::GmDlsSample => encode(GmDlsSample, 0),
		Instruction::AddSub => encode(AddSub, 0),
		Instruction::Random => encode(Random, 0),
		Instruction::BufferLoadWithOffset => encode(BufferLoadWithOffset, 0),
		Instruction::BufferLoad => encode(BufferLoad, 0),
		Instruction::BufferStoreAndStep => encode(BufferStoreAndStep, 0),
		Instruction::BufferAlloc => encode(BufferAlloc, 0),
		Instruction::CallInstrument => encode(CallInstrument, 0),
		Instruction::Call(proc) => encode(ProcCall, proc),
		Instruction::Constant(constant) => encode_constant(constant),
		Instruction::SampleRate => encode_constant(sample_rate.to_bits()),
		Instruction::StackLoad(offset) => encode(StackLoad, offset),
		Instruction::StackStore(offset) => encode(StackStore, offset),
		Instruction::CellStore(offset) => encode(CellStore, offset),
		Instruction::Label => encode(Label, 0),
		Instruction::If => encode(If, 0),
		Instruction::Loop => encode(LoopJump, 0),
		Instruction::EndIf => encode(EndIf, 0),
		Instruction::Else => encode(Else, 0),

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
		Instruction::Mlog2 => {
			encode(Fputnext, 0);
			encode_fop(Fyl2x, encode);
			encode(Fdone, 0);
		},
		Instruction::Sin => {
			encode_fop(Fsin, encode);
			encode(Fdone, 0);
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
		Instruction::ExpandStereo => encode_implicit(ExpandL, encode),
		Instruction::ExpandGeneric => encode_implicit(ExpandL, encode),
		Instruction::Pop => encode_implicit(Pop, encode),
		Instruction::PopNext => encode_implicit(PopNext, encode),
		Instruction::BufferIndex => encode_implicit(BufferIndexAndLength, encode),
		Instruction::BufferLength => {
			encode_implicit(BufferIndexAndLength, encode);
			encode_implicit(ExpandR, encode);
		},
		Instruction::Cmp => encode_implicit(Cmp, encode),
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

pub fn encode_bytecodes(program: &ZingProgram, sample_rate: f32) -> Result<(Vec<u8>, Vec<u32>), String> {
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
	for &inst in program.procedures.iter().map(|p| &p.code).flatten() {
		encode_bytecode(inst, sample_rate, &mut discover_opcode, &mut discover_constant);
	}
	opcode_capacity[EncodedBytecode::Constant as usize] = constant_set.len() as u16 + 1;

	// Build constant list
	let constants: Vec<u32> = constant_set.into_iter().collect();
	let mut constant_map = BTreeMap::new();
	for (i, &v) in constants.iter().enumerate() {
		constant_map.insert(v, i as u16);
	}

	adjust_to_fixed_capacities(&mut opcode_capacity)?;

	let opcode_space = opcode_capacity.iter().sum::<u16>() + 1;
	if opcode_space > 256 {
		let error = format!("\nExceeded opcode space ({} > {}).", opcode_space, 256);
		return Err(error);
	}

	// Compute opcode base values
	let mut opcode_base = opcode_capacity;
	let mut base = 0x100;
	for opcode in 0 .. EncodedBytecode::Implicit as usize {
		base -= opcode_base[opcode];
		opcode_base[opcode] = base;
	}
	opcode_base[EncodedBytecode::Implicit as usize] = 0x01;

	// Perform the encoding
	let encoded = RefCell::new(vec![]);
	let mut encode_opcode = |opcode: EncodedBytecode, arg: u16| {
		encoded.borrow_mut().push(opcode_base[opcode as usize].wrapping_add(arg) as u8);
	};
	let mut encode_constant = |value: u32| {
		let opcode = EncodedBytecode::Constant;
		let arg = constant_map[&value];
		encoded.borrow_mut().push(opcode_base[opcode as usize].wrapping_add(arg) as u8);
	};
	for procedure in &program.procedures {
		encode_opcode(EncodedBytecode::Proc, 0);
		for &inst in &procedure.code {
			encode_bytecode(inst, sample_rate, &mut encode_opcode, &mut encode_constant);
		}
	}
	encode_opcode(EncodedBytecode::Proc, 0);

	Ok((encoded.into_inner(), constants))
}

fn adjust_to_fixed_capacities(opcode_capacity: &mut Vec<u16>) -> Result<(), String> {
	for capacity in opcode_capacity.iter_mut() {
		if *capacity == 0 { *capacity = 1; }
	}

	let mut exceeded = BTreeMap::new();
	let mut adjust = |opcode: EncodedBytecode, fixed_capacity: u16, name: &'static str| {
		let capacity = replace(&mut opcode_capacity[opcode as usize], fixed_capacity);
		if capacity > fixed_capacity {
			exceeded.entry(name).and_modify(|existing: &mut (u16, u16)| {
				existing.0 = existing.0.max(capacity)
			}).or_insert((capacity, fixed_capacity));
		}
	};

	adjust(EncodedBytecode::Fop, 15, "fop");
	adjust(EncodedBytecode::ProcCall, 75, "procedure");
	adjust(EncodedBytecode::Constant, 50, "constant");
	adjust(EncodedBytecode::ReadNoteProperty, 3, "note property");
	adjust(EncodedBytecode::StackLoad, 20, "stack load");
	adjust(EncodedBytecode::StackStore, 20, "stack store");
	adjust(EncodedBytecode::CellStore, 10, "cell store");
	adjust(EncodedBytecode::Round, 4, "round");
	adjust(EncodedBytecode::Compare, 7, "compare");

	if !exceeded.is_empty() {
		let mut errors = String::new();
		for (name, (capacity, fixed_capacity)) in exceeded {
			errors += &format!("\nExceeded {} capacity ({} > {}).", name, capacity, fixed_capacity);
		}
		return Err(errors);
	}

	Ok(())
}
