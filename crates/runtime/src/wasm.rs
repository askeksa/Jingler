use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use walrus::*;
use walrus::ir::*;
use wasmtime::{Engine, Linker, Module, Store, TypedFunc};

use crate::JinglerRuntime;
use ::ir;

const STATE_ADDRESS: i32 = 0;

pub struct WasmRuntime {
	engine: Engine,
	linker: Linker<Arc<WasmRuntimeData>>,
	store: Store<Arc<WasmRuntimeData>>,
	program: Option<WasmProgram>,
}

struct WasmRuntimeData {}

struct WasmProgram {
	program: ir::Program,
	sample_rate: f32,
	module: Module,
	initialize_func: TypedFunc<f32, ()>,
	note_on_func: TypedFunc<(i32, i32, i32), ()>,
	note_off_func: TypedFunc<(i32, i32), ()>,
	next_sample_func: TypedFunc<(), (f64, f64)>,
}

impl WasmRuntime {
	pub fn new() -> Result<Self> {
		let engine = Engine::default();
		let linker = Linker::new(&engine);
		let data = Arc::new(WasmRuntimeData {});
		let store = Store::new(&engine, data.clone());

		Ok(Self {
			engine,
			linker,
			store,
			program: None,
		})
	}
}

impl JinglerRuntime for WasmRuntime {
	fn load_program(&mut self, program: &ir::Program, sample_rate: f32) -> Result<()> {
		let wasm = compile_to_wasm(program, sample_rate)?;
		let module = Module::from_binary(&self.engine, &wasm)?;
		let instance = self.linker.instantiate(&mut self.store, &module)?;
		let initialize_func = instance.get_typed_func::<f32, ()>(&mut self.store, "initialize")?;
		let note_on_func = instance.get_typed_func::<(i32, i32, i32), ()>(&mut self.store, "note_on")?;
		let note_off_func = instance.get_typed_func::<(i32, i32), ()>(&mut self.store, "note_off")?;
		let next_sample_func = instance.get_typed_func::<(), (f64, f64)>(&mut self.store, "next_sample")?;

		self.program = Some(WasmProgram {
			program: program.clone(),
			sample_rate,
			module,
			initialize_func,
			note_on_func,
			note_off_func,
			next_sample_func,
		});

		self.reset()
	}

	fn unload_program(&mut self) {
		self.program.take();
	}

	fn reset(&mut self) -> Result<()> {
		if let Some(program) = &self.program {
			program.initialize_func.call(&mut self.store, program.sample_rate)?;
		}
		Ok(())
	}

	fn next_sample(&mut self) -> Result<[f64; 2]> {
		let (l, r) = if let Some(program) = &self.program {
			program.next_sample_func.call(&mut self.store, ())?
		} else {
			(0.0, 0.0)
		};
		Ok([l, r])
	}

	fn note_on(&mut self, channel: u8, note: u8, velocity: u8) -> Result<()> {
		if let Some(program) = &self.program {
			program.note_on_func.call(&mut self.store, (channel as i32, note as i32, velocity as i32))?;
		}
		Ok(())
	}

	fn note_off(&mut self, channel: u8, note: u8) -> Result<()> {
		if let Some(program) = &self.program {
			program.note_off_func.call(&mut self.store, (channel as i32, note as i32))?;
		}
		Ok(())
	}

	fn set_parameter(&mut self, _index: usize, _value: f32) -> Result<()> {
		// TODO
		Ok(())
	}
}

fn compile_to_wasm(program: &ir::Program, sample_rate: f32) -> Result<Vec<u8>> {
	let config = ModuleConfig::new();
	let mut module = walrus::Module::with_config(config);
	let memory = module.memories.add_local(false, false, 16, Some(16), None);
	let sample_rate = module.globals.add_local(ValType::F32, true, false, ConstExpr::Value(Value::F32(sample_rate)));
	let state_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));

	let mut generator = WasmGenerator {
		program,
		module: RefCell::new(module),
		memory,
		sample_rate,
		state_ptr,
		function_id_map: HashMap::new(),
	};

	generator.generate()?;

	Ok(generator.module().emit_wasm())
}

struct WasmGenerator<'ir> {
	program: &'ir ir::Program,
	module: RefCell<walrus::Module>,
	memory: MemoryId,
	sample_rate: GlobalId,
	state_ptr: GlobalId,

	function_id_map: HashMap<u16, FunctionId>,
}

impl<'ir> WasmGenerator<'ir> {
	fn module(&self) -> RefMut<'_, walrus::Module> {
		self.module.borrow_mut()
	}

	fn generate(&mut self) -> Result<()> {
		let initialize_function_id = self.generate_initialize_function();
		self.module().exports.add("initialize", initialize_function_id);

		let note_on_function_id = self.generate_note_on_function();
		self.module().exports.add("note_on", note_on_function_id);

		let note_off_function_id = self.generate_note_off_function();
		self.module().exports.add("note_off", note_off_function_id);

		let next_sample_function_id = self.generate_next_sample_function();
		self.module().exports.add("next_sample", next_sample_function_id);

		Ok(())
	}

	fn get_function_id(&mut self, proc_id: u16) -> FunctionId {
		if let Some(&id) = self.function_id_map.get(&proc_id) {
			id
		} else {
			let id = self.generate_function_for_procedure(proc_id);
			self.function_id_map.insert(proc_id, id);
			id
		}
	}

	fn generate_function_for_procedure(&mut self, proc_id: u16) -> FunctionId {
		let procedure = &self.program.procedures[proc_id as usize];

		// First scan for calls and generate all callees
		let mut callees = HashMap::new();
		for instr in &procedure.code {
			use ir::Instruction::*;
			match instr {
				Call(proc_id, ..) => {
					callees.entry(*proc_id).or_insert_with(|| self.get_function_id(*proc_id));
				}
				PlayInstrument(static_proc_id, dynamic_proc_id) => {
					callees.entry(*static_proc_id).or_insert_with(|| self.get_function_id(*static_proc_id));
					callees.entry(*dynamic_proc_id).or_insert_with(|| self.get_function_id(*dynamic_proc_id));
				}
				_ => {}
			}
		}

		// Create builder
		let params = procedure.inputs.iter().map(|_| ValType::V128).collect::<Vec<_>>();
		let results = procedure.outputs.iter().map(|_| ValType::V128).collect::<Vec<_>>();
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		builder.name(format!("{}: {}", procedure.kind, procedure.name));
		let instr_builder = RefCell::new(builder.func_body());
		let b = || instr_builder.borrow_mut();

		// Cell stack for cell push/fetch/pop
		let mut cell_stack = Vec::new();

		// Operand stack and helpers
		let operand_stack = RefCell::new(args.clone());
		let pop = || {
			b().local_get(operand_stack.borrow_mut().pop().unwrap());
		};
		let push = || {
			let local = self.module().locals.add(ValType::V128);
			operand_stack.borrow_mut().push(local);
			b().local_set(local);
		};
		let op = |pops: usize, pushes: usize, action: &mut dyn FnMut()| {
			for _ in 0..pops {
				pop();
			}
			action();
			for _ in 0..pushes {
				push();
			}
		};

		// Translate instructions
		for instr in &procedure.code {
			use ir::Instruction::*;
			match *instr {
				Expand(_) => {},
				StackLoad(index) => {
					let mut stack = operand_stack.borrow_mut();
					let index = stack.len() - 1 - (index as usize);
					let local = stack[index];
					stack.push(local);
				},
				StackStore(index) => {
					let mut stack = operand_stack.borrow_mut();
					let local = stack.pop().unwrap();
					let index = stack.len() - 1 - (index as usize);
					stack[index] = local;
				},
				Pop => {
					let mut stack = operand_stack.borrow_mut();
					stack.pop();
				},
				PopNext => {
					let mut stack = operand_stack.borrow_mut();
					let local = stack.pop().unwrap();
					stack.pop();
					stack.push(local);
				},

				Call(proc_id, ..) => {
					let procedure = &self.program.procedures[proc_id as usize];
					let id = callees[&proc_id];

					let mut stack = operand_stack.borrow_mut();
					let split_point = stack.len() - procedure.inputs.len();
					let inputs = stack.split_off(split_point);
					for input in inputs {
						b().local_get(input);
					}
					b().call(id);
					let outputs = procedure.outputs.iter().map(|_| self.module().locals.add(ValType::V128)).collect::<Vec<_>>();
					for output in outputs.iter().rev() {
						b().local_set(*output);
					}
					stack.extend(outputs);
				},

				Add => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Add); }),
				Sub => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Sub); }),
				Mul => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Mul); }),
				Div => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Div); }),
				And => op(2, 1, &mut || { b().binop(BinaryOp::V128And); }),
				Or => op(2, 1, &mut || { b().binop(BinaryOp::V128Or); }),
				Xor => op(2, 1, &mut || { b().binop(BinaryOp::V128Xor); }),
				Min => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Min); }),
				Max => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Max); }),
				Eq => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Eq); }),
				Greater => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Gt); }),
				GreaterEq => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Ge); }),
				Less => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Lt); }),
				LessEq => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Le); }),
				Neq => op(2, 1, &mut || { b().binop(BinaryOp::F64x2Ne); }),
				Ceil => op(1, 1, &mut || { b().unop(UnaryOp::F64x2Ceil); }),
				Floor => op(1, 1, &mut || { b().unop(UnaryOp::F64x2Floor); }),
				Round => op(1, 1, &mut || { b().unop(UnaryOp::F64x2Nearest); }),
				Trunc => op(1, 1, &mut || { b().unop(UnaryOp::F64x2Trunc); }),
				Sqrt => op(1, 1, &mut || { b().unop(UnaryOp::F64x2Sqrt); }),

				Constant(value) => op(0, 1, &mut || {
					b().f32_const(f32::from_bits(value));
					b().unop(UnaryOp::F64PromoteF32);
					b().unop(UnaryOp::F64x2Splat);
				}),
				SampleRate => op(0, 1, &mut || {
					b().global_get(self.sample_rate);
					b().unop(UnaryOp::F64PromoteF32);
					b().unop(UnaryOp::F64x2Splat);
				}),
				Left => op(1, 1, &mut || {
					b().unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b().unop(UnaryOp::F64x2Splat);
				}),
				Right => op(1, 1, &mut || {
					b().unop(UnaryOp::F64x2ExtractLane { idx: 1 });
					b().unop(UnaryOp::F64x2Splat);
				}),
				MergeLR => op(2, 1, &mut || {
					b().unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b().binop(BinaryOp::F64x2ReplaceLane { idx: 1 });
				}),

				AndNot => {
					let (s0, s1) = {
						let mut stack = operand_stack.borrow_mut();
						(stack.pop().unwrap(), stack.pop().unwrap())
					};
					b().local_get(s1);
					b().local_get(s0);
					b().binop(BinaryOp::V128AndNot);
					push();
				},
				AddSub => {
					let (s0, s1) = {
						let mut stack = operand_stack.borrow_mut();
						(stack.pop().unwrap(), stack.pop().unwrap())
					};
					b().local_get(s0);
					b().local_get(s1);
					b().local_get(s1);
					b().unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b().unop(UnaryOp::F64Neg);
					b().binop(BinaryOp::F64x2ReplaceLane { idx: 0 });
					b().binop(BinaryOp::F64x2Add);
					push();
				},
				SplitRL => {
					let loc = operand_stack.borrow_mut().pop().unwrap();
					b().local_get(loc);
					b().unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b().unop(UnaryOp::F64x2Splat);
					push();
					b().local_get(loc);
					b().unop(UnaryOp::F64x2ExtractLane { idx: 1 });
					b().unop(UnaryOp::F64x2Splat);
					push();
				},

				CellInit => {
					b().global_get(self.state_ptr);
					b().global_get(self.state_ptr);
					b().i32_const(16);
					b().binop(BinaryOp::I32Add);
					b().global_set(self.state_ptr);
					pop();
					b().store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });
				},
				CellRead => {
					b().global_get(self.state_ptr);
					b().global_get(self.state_ptr);
					b().i32_const(16);
					b().binop(BinaryOp::I32Add);
					b().global_set(self.state_ptr);
					b().load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push();
				},
				CellPush => {
					let state_addr = self.module().locals.add(ValType::I32);
					cell_stack.push(state_addr);
					b().global_get(self.state_ptr);
					b().local_tee(state_addr);
					b().global_get(self.state_ptr);
					b().i32_const(16);
					b().binop(BinaryOp::I32Add);
					b().global_set(self.state_ptr);
					b().load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push();
				},
				CellFetch => {
					let state_addr = *cell_stack.last().unwrap();
					b().local_get(state_addr);
					b().load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push();
				},
				CellPop => {
					let state_addr = cell_stack.pop().unwrap();
					b().local_get(state_addr);
					pop();
					b().store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });
				},

				_ => panic!("Unsupported instruction: {:?}", instr),
			}
		}

		// Return values on stack
		for local in operand_stack.borrow().iter() {
			b().local_get(*local);
		}

		// Finish function
		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_initialize_function(&mut self) -> FunctionId {
		let params = [ValType::F32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		let mut b = builder.func_body();

		b.i32_const(STATE_ADDRESS);
		b.global_set(self.state_ptr);
		b.call(self.get_function_id(self.program.main_static_proc_id as u16));

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_note_on_function(&mut self) -> FunctionId {
		let params = [ValType::I32, ValType::I32, ValType::I32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_note_off_function(&mut self) -> FunctionId {
		let params = [ValType::I32, ValType::I32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_next_sample_function(&mut self) -> FunctionId {
		let params = [];
		let results = [ValType::F64, ValType::F64];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		let mut b = builder.func_body();

		b.i32_const(STATE_ADDRESS);
		b.global_set(self.state_ptr);
		b.call(self.get_function_id(self.program.main_dynamic_proc_id as u16));
		let result_local = self.module().locals.add(ValType::V128);
		b.local_tee(result_local);
		b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
		b.local_get(result_local);
		b.unop(UnaryOp::F64x2ExtractLane { idx: 1 });

		builder.finish(args, &mut self.module().funcs)
	}
}
