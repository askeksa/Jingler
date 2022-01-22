
use std::collections::{BTreeMap, BTreeSet};
use std::mem::replace;

use crate::bytecodes::Bytecode;


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
	NoteProperty,
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

fn encode_bytecode(bc: Bytecode, constant_map: &BTreeMap<u32, u16>, sample_rate: f32,
                   encode: &mut impl FnMut(EncodedBytecode, u16)) {
	use EncodedBytecode::*;
	use EncodedFop::*;
	use EncodedRoundingMode::*;
	use EncodedCompareOp::*;
	use EncodedImplicit::*;
	match bc {
		Bytecode::StateEnter => encode(StateEnter, 0),
		Bytecode::StateLeave => encode(StateLeave, 0),
		Bytecode::Kill => encode(Kill, 0),
		Bytecode::CellInit => encode(CellInit, 0),
		Bytecode::CellRead => encode(CellRead, 0),
		Bytecode::GmDlsLength => encode(GmDlsLength, 0),
		Bytecode::GmDlsSample => encode(GmDlsSample, 0),
		Bytecode::AddSub => encode(AddSub, 0),
		Bytecode::Random => encode(Random, 0),
		Bytecode::BufferLoadWithOffset => encode(BufferLoadWithOffset, 0),
		Bytecode::BufferLoad => encode(BufferLoad, 0),
		Bytecode::BufferStoreAndStep => encode(BufferStoreAndStep, 0),
		Bytecode::BufferAlloc => encode(BufferAlloc, 0),
		Bytecode::CallInstrument => encode(CallInstrument, 0),
		Bytecode::Proc => encode(Proc, 0),
		Bytecode::Call(proc) => encode(ProcCall, proc),
		Bytecode::Constant(constant) => encode(Constant, constant_map[&constant]),
		Bytecode::SampleRate => encode(Constant, constant_map[&sample_rate.to_bits()]),
		Bytecode::ReadNoteProperty(property) => encode(NoteProperty, property as u16),
		Bytecode::StackLoad(offset) => encode(StackLoad, offset),
		Bytecode::StackStore(offset) => encode(StackStore, offset),
		Bytecode::CellStore(offset) => encode(CellStore, offset),
		Bytecode::Label => encode(Label, 0),
		Bytecode::If => encode(If, 0),
		Bytecode::Loop => encode(LoopJump, 0),
		Bytecode::EndIf => encode(EndIf, 0),
		Bytecode::Else => encode(Else, 0),

		Bytecode::Ceil => encode_rounding(Ceil, encode),
		Bytecode::Floor => encode_rounding(Floor, encode),
		Bytecode::Round => encode_rounding(Nearest, encode),
		Bytecode::Trunc => encode_rounding(Truncate, encode),

		Bytecode::Eq => encode_comparison(Eq, encode),
		Bytecode::Greater => encode_comparison(Greater, encode),
		Bytecode::GreaterEq => encode_comparison(GreaterEq, encode),
		Bytecode::Less => encode_comparison(Less, encode),
		Bytecode::LessEq => encode_comparison(LessEq, encode),
		Bytecode::Neq => encode_comparison(Neq, encode),

		Bytecode::Atan2 => {
			encode(Fputnext, 0);
			encode_fop(Fpatan, encode);
			encode(Fdone, 0);
		},
		Bytecode::Cos => {
			encode_fop(Fcos, encode);
			encode(Fdone, 0);
		},
		Bytecode::Exp2 => {
			encode_fop(Frndint, encode);
			encode(Exp2Body, 0);
			encode(Fdone, 0);
		},
		Bytecode::Mlog2 => {
			encode(Fputnext, 0);
			encode_fop(Fyl2x, encode);
			encode(Fdone, 0);
		},
		Bytecode::Sin => {
			encode_fop(Fsin, encode);
			encode(Fdone, 0);
		},
		Bytecode::Tan => {
			encode_fop(Fptan, encode);
			encode(Fdone, 0);
			encode(Fdone, 0);
		},

		Bytecode::MergeLR => encode_implicit(MergeLR, encode),
		Bytecode::SplitRL => encode_implicit(SplitRL, encode),
		Bytecode::ExpandL => encode_implicit(ExpandL, encode),
		Bytecode::ExpandR => encode_implicit(ExpandR, encode),
		Bytecode::Pop => encode_implicit(Pop, encode),
		Bytecode::PopNext => encode_implicit(PopNext, encode),
		Bytecode::BufferIndexAndLength => encode_implicit(BufferIndexAndLength, encode),
		Bytecode::Cmp => encode_implicit(Cmp, encode),
		Bytecode::Sqrt => encode_implicit(Sqrt, encode),
		Bytecode::And => encode_implicit(And, encode),
		Bytecode::AndNot => encode_implicit(AndNot, encode),
		Bytecode::Or => encode_implicit(Or, encode),
		Bytecode::Xor => encode_implicit(Xor, encode),
		Bytecode::Add => encode_implicit(Add, encode),
		Bytecode::Mul => encode_implicit(Mul, encode),
		Bytecode::Sub => encode_implicit(Sub, encode),
		Bytecode::Min => encode_implicit(Min, encode),
		Bytecode::Div => encode_implicit(Div, encode),
		Bytecode::Max => encode_implicit(Max, encode),
	}
}

pub fn encode_bytecodes(bytecodes: &[Bytecode], sample_rate: f32) -> Result<(Vec<u8>, Vec<u32>), String> {
	// Build constant list
	let mut constant_set = BTreeSet::new();
	for bc in bytecodes {
		match bc {
			Bytecode::Constant(v) => {
				constant_set.insert(*v);
			},
			Bytecode::SampleRate => {
				constant_set.insert(sample_rate.to_bits());
			},
			_ => {},
		}
	}
	let constants: Vec<u32> = constant_set.into_iter().collect();

	let mut constant_map = BTreeMap::new();
	for (i, &v) in constants.iter().enumerate() {
		constant_map.insert(v, i as u16);
	}

	// Collect arg space for each opcode
	let mut opcode_capacity = vec![0u16; EncodedBytecode::Implicit as usize + 1];
	let mut discover_opcode = |opcode: EncodedBytecode, arg: u16| {
		let capacity = &mut opcode_capacity[opcode as usize];
		*capacity = (*capacity).max(arg + 1);
	};
	for &bc in bytecodes {
		encode_bytecode(bc, &constant_map, sample_rate, &mut discover_opcode);
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
	let mut encoded: Vec<u8> = vec![];
	let mut encode_opcode = |opcode: EncodedBytecode, arg: u16| {
		encoded.push(opcode_base[opcode as usize].wrapping_add(arg) as u8);
	};
	for &bc in bytecodes {
		encode_bytecode(bc, &constant_map, sample_rate, &mut encode_opcode);
	}

	Ok((encoded, constants))
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
	adjust(EncodedBytecode::NoteProperty, 3, "note property");
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
