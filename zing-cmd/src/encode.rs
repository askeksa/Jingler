
use std::collections::{BTreeMap, BTreeSet};

use zing::bytecodes::Bytecode;


const fn implicit_code(opcode: u8) -> u8 {
	opcode - (1 << (7 - opcode.leading_zeros())) - 3
}


#[allow(non_snake_case)]
pub fn encode_bytecodes(bytecodes: &[Bytecode]) -> Result<(Vec<u8>, Vec<u32>), String> {
	let mut constant_set = BTreeSet::new();
	for bc in bytecodes {
		if let Bytecode::Constant(v) = bc {
			constant_set.insert(*v);
		}
	}
	let constants: Vec<u32> = constant_set.into_iter().collect();

	let mut constant_map = BTreeMap::new();
	for (i, v) in constants.iter().enumerate() {
		constant_map.insert(v, i as u16);
	}

	let mut base = 0x100;
	let mut next = |n| -> u8 {
		base -= n;
		base as u8
	};

	let STATE_ENTER = next(1);
	let STATE_LEAVE = next(1);
	let KILL = next(1);
	let CELL_INIT = next(1);
	let CELL_READ = next(1);
	let ADDSUB = next(1);
	let FPUTNEXT = next(1);
	let RANDOM = next(1);
	let BUFFER_STORE = next(1);
	let BUFFER_LOAD = next(1);
	let BUFFER_ALLOC = next(1);
	let CALL_INSTRUMENT = next(1);
	let EXP2_BODY = next(1);
	let FDONE = next(1);
	let FOP = next(15);
	let PROC = next(1);
	let PROC_CALL = next(50);
	let CONSTANT = next(50);
	let NOTE_PROPERTY = next(3);
	let STACK_LOAD = next(25);
	let STACK_STORE = next(25);
	let CELL_STORE = next(25);
	let LABEL = next(1);
	let IF = next(1);
	let LOOPJUMP = next(1);
	let ENDIF = next(1);
	let ELSE = next(1);
	let ROUND = next(4);
	let COMPARE = next(7);

	fn check(value: u16, capacity: u16, name: &str) -> Result<u8, String> {
		if value < capacity {
			Ok(value as u8)
		} else {
			Err(format!("Exceeded {} capacity.", name))
		}
	}

	let mut encoded = vec![];
	for &bc in bytecodes {
		let e = match bc {
			Bytecode::StateEnter => STATE_ENTER,
			Bytecode::StateLeave => STATE_LEAVE,
			Bytecode::Kill => KILL,
			Bytecode::CellInit => CELL_INIT,
			Bytecode::CellRead => CELL_READ,
			Bytecode::AddSub => ADDSUB,
			Bytecode::Fputnext => FPUTNEXT,
			Bytecode::Random => RANDOM,
			Bytecode::BufferStore => BUFFER_STORE,
			Bytecode::BufferLoad => BUFFER_LOAD,
			Bytecode::BufferAlloc => BUFFER_ALLOC,
			Bytecode::CallInstrument => CALL_INSTRUMENT,
			Bytecode::Exp2Body => EXP2_BODY,
			Bytecode::Fdone => FDONE,
			Bytecode::Fop(opcode) => FOP + (0xFF - opcode as u8),
			Bytecode::Proc => PROC,
			Bytecode::Call(proc) => PROC_CALL + check(proc, 50, "procedure")?,
			Bytecode::Constant(constant) => CONSTANT + check(constant_map[&constant], 50, "constant")?,
			Bytecode::ReadNoteProperty(property) => NOTE_PROPERTY + property as u8,
			Bytecode::StackLoad(offset) => STACK_LOAD + check(offset, 25, "stack load")?,
			Bytecode::StackStore(offset) => STACK_STORE + check(offset, 25, "stack store")?,
			Bytecode::CellStore(offset) => CELL_STORE + check(offset, 25, "cell store")?,
			Bytecode::Label => LABEL,
			Bytecode::If => IF,
			Bytecode::Loop => LOOPJUMP,
			Bytecode::EndIf => ENDIF,
			Bytecode::Else => ELSE,
			Bytecode::Round(mode) => ROUND + mode as u8,
			Bytecode::Compare(op) => COMPARE + op as u8,

			Bytecode::Expand => implicit_code(0x14),
			Bytecode::SplitRL => implicit_code(0x15),
			Bytecode::MergeLR => implicit_code(0x16),
			Bytecode::SplitLR => implicit_code(0x17),
			Bytecode::Pop => implicit_code(0x28),
			Bytecode::PopNext => implicit_code(0x29),
			Bytecode::BufferLength => implicit_code(0x2A),
			Bytecode::Cmp => implicit_code(0x2E),
			Bytecode::Sqrt => implicit_code(0x51),
			Bytecode::And => implicit_code(0x54),
			Bytecode::AndNot => implicit_code(0x55),
			Bytecode::Or => implicit_code(0x56),
			Bytecode::Xor => implicit_code(0x57),
			Bytecode::Add => implicit_code(0x58),
			Bytecode::Mul => implicit_code(0x59),
			Bytecode::Sub => implicit_code(0x5C),
			Bytecode::Min => implicit_code(0x5D),
			Bytecode::Div => implicit_code(0x5E),
			Bytecode::Max => implicit_code(0x5F),
		};
		encoded.push(e);
	}

	Ok((encoded, constants))
}
