use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use walrus::*;
use walrus::ir::*;
use wasmtime::{Caller, Engine, Linker, Module, Store, TypedFunc};

use crate::JinglerRuntime;
use ::ir;

const RANDOM_SCRAMBLE: i64 = 0x42118159;

const GMDLS_OFFSETS: usize = 0x43E3A;
const GMDLS_DATA: usize = 0x4462C;
const GMDLS_COUNT: usize = 495;

const PARAMETER_ADDRESS: i32 = 0;
const NOTE_LISTS_ADDRESS: i32 = 0x1000;
const NOTE_COMMANDS_ADDRESS: i32 = 0x2000;
const STATE_ADDRESS: i32 = 0x10000;
const BUFFER_BASE_ADDRESS: i32 = 0x100000; // 1MB into Wasm memory
const INITIAL_MEMORY_SIZE: u64 = BUFFER_BASE_ADDRESS as u64; // 1MB
const MAX_MEMORY_SIZE: u64 = 0x40000000; // 1GB

/// Sentinel track value marking end of note commands
const COMMAND_SENTINEL: i32 = 0x7FFFFFFF;

pub struct WasmRuntime {
	engine: Engine,
	linker: Linker<Arc<WasmRuntimeData>>,
	store: Store<Arc<WasmRuntimeData>>,
	program: Option<WasmProgram>,
}

struct WasmRuntimeData {
	gmdls_data: Vec<u8>,
}

struct WasmProgram {
	program: ir::Program,
	sample_rate: f32,
	initialize_func: TypedFunc<f32, ()>,
	note_on_func: TypedFunc<(i32, i32, i32), ()>,
	note_off_func: TypedFunc<(i32, i32), ()>,
	next_sample_func: TypedFunc<(), (f64, f64)>,
	set_parameter_func: TypedFunc<(i32, f32), ()>,
}

impl WasmRuntime {
	pub fn new() -> Result<Self> {
		let engine = Engine::default();
		let mut linker = Linker::new(&engine);

		linker.func_wrap("math", "atan2", |y: f64, x: f64| -> f64 { y.atan2(x) })?;
		linker.func_wrap("math", "cos", |x: f64| -> f64 { x.cos() })?;
		linker.func_wrap("math", "exp2", |x: f64| -> f64 { x.exp2() })?;
		linker.func_wrap("math", "log2", |x: f64| -> f64 { x.log2() })?;
		linker.func_wrap("math", "pow", |x: f64, y: f64| -> f64 { x.powf(y) })?;
		linker.func_wrap("math", "sin", |x: f64| -> f64 { x.sin() })?;
		linker.func_wrap("math", "sincos", |x: f64| -> (f64, f64) { x.sin_cos() })?;
		linker.func_wrap("math", "tan", |x: f64| -> f64 { x.tan() })?;

		linker.func_wrap("gmdls", "sample", |caller: Caller<'_, Arc<WasmRuntimeData>>, sound_id: i32, index: i32| -> i32 {
			gmdls_lookup(&caller.data().gmdls_data, sound_id, index)
		})?;

		let gmdls_data = load_gmdls();
		let data = Arc::new(WasmRuntimeData { gmdls_data });
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
		let set_parameter_func = instance.get_typed_func::<(i32, f32), ()>(&mut self.store, "set_parameter")?;

		self.program = Some(WasmProgram {
			program: program.clone(),
			sample_rate,
			initialize_func,
			note_on_func,
			note_off_func,
			next_sample_func,
			set_parameter_func,
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
			for (track, track_channel) in program.program.track_order.iter().enumerate() {
				if *track_channel == channel as usize {
					program.note_on_func.call(&mut self.store, (track as i32, note as i32, velocity as i32))?;
				}
			}
		}
		Ok(())
	}

	fn note_off(&mut self, channel: u8, note: u8) -> Result<()> {
		if let Some(program) = &self.program {
			for (track, track_channel) in program.program.track_order.iter().enumerate() {
				if *track_channel == channel as usize {
					program.note_off_func.call(&mut self.store, (track as i32, note as i32))?;
				}
			}
		}
		Ok(())
	}

	fn set_parameter(&mut self, index: usize, value: f32) -> Result<()> {
		if let Some(p) = &self.program {
			if index < p.program.parameters.len() {
				let param = &p.program.parameters[index];
				let quant_value = param.min + value * (param.max - param.min);
				p.set_parameter_func.call(&mut self.store, (index as i32, quant_value))?;
			}
		}
		Ok(())
	}
}

fn compile_to_wasm(program: &ir::Program, sample_rate: f32) -> Result<Vec<u8>> {
	let config = ModuleConfig::new();
	let mut module = walrus::Module::with_config(config);
	let memory = module.memories.add_local(false, false, INITIAL_MEMORY_SIZE >> 16, Some(MAX_MEMORY_SIZE >> 16), None);
	let sample_rate = module.globals.add_local(ValType::F32, true, false, ConstExpr::Value(Value::F32(sample_rate)));
	let state_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));
	let buffer_alloc_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(BUFFER_BASE_ADDRESS)));
	let track_command_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(NOTE_COMMANDS_ADDRESS)));
	let track_index = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));
	let note_alloc_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));
	let note_ptr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));
	let prev_link_addr = module.globals.add_local(ValType::I32, true, false, ConstExpr::Value(Value::I32(0)));

	let f64_to_f64 = module.types.add(&[ValType::F64], &[ValType::F64]);
	let f64_f64_to_f64 = module.types.add(&[ValType::F64, ValType::F64], &[ValType::F64]);
	let f64_to_f64_f64 = module.types.add(&[ValType::F64], &[ValType::F64, ValType::F64]);
	let i32_i32_to_i32 = module.types.add(&[ValType::I32, ValType::I32], &[ValType::I32]);

	let (atan2, _) = module.add_import_func("math", "atan2", f64_f64_to_f64);
	let (cos, _) = module.add_import_func("math", "cos", f64_to_f64);
	let (exp2, _) = module.add_import_func("math", "exp2", f64_to_f64);
	let (log2, _) = module.add_import_func("math", "log2", f64_to_f64);
	let (pow, _) = module.add_import_func("math", "pow", f64_f64_to_f64);
	let (sin, _) = module.add_import_func("math", "sin", f64_to_f64);
	let (sincos, _) = module.add_import_func("math", "sincos", f64_to_f64_f64);
	let (tan, _) = module.add_import_func("math", "tan", f64_to_f64);
	let (gmdls_sample_fn, _) = module.add_import_func("gmdls", "sample", i32_i32_to_i32);

	let math = MathImports { sin, cos, tan, exp2, log2, pow, atan2, sincos };

	let folded_multiply_fn = generate_folded_multiply_fn(&mut module);
	let buffer_alloc_fn = generate_buffer_alloc_fn(&mut module, memory, buffer_alloc_ptr);

	let mut generator = WasmGenerator {
		program,
		module: RefCell::new(module),
		memory,
		sample_rate,
		state_ptr,
		buffer_alloc_ptr,
		track_command_ptr,
		track_index,
		note_alloc_ptr,
		note_ptr,
		prev_link_addr,
		math,
		folded_multiply_fn,
		buffer_alloc_fn,
		gmdls_sample_fn,
		function_id_map: HashMap::new(),
	};

	generator.generate()?;

	Ok(generator.module().emit_wasm())
}

/// Build folded_multiply(val: i32) -> i32:
///   product = (val as u64) * RANDOM_SCRAMBLE
///   return (product as i32) ^ ((product >> 32) as i32)
fn generate_folded_multiply_fn(module: &mut walrus::Module) -> FunctionId {
	let params = [ValType::I32];
	let results = [ValType::I32];
	let args: Vec<LocalId> = params.iter().map(|p| module.locals.add(*p)).collect();
	let mut builder = FunctionBuilder::new(&mut module.types, &params, &results);
	builder.name("folded_multiply".to_string());
	let product = module.locals.add(ValType::I64);
	let mut b = builder.func_body();
	b.local_get(args[0]);
	b.unop(UnaryOp::I64ExtendUI32);
	b.i64_const(RANDOM_SCRAMBLE);
	b.binop(BinaryOp::I64Mul);
	b.local_tee(product);
	b.unop(UnaryOp::I32WrapI64);
	b.local_get(product);
	b.i64_const(32);
	b.binop(BinaryOp::I64ShrU);
	b.unop(UnaryOp::I32WrapI64);
	b.binop(BinaryOp::I32Xor);
	builder.finish(args, &mut module.funcs)
}

/// Build buffer_alloc(size: i32) -> v128:
///   ptr = buffer_alloc_ptr
///   buffer_alloc_ptr += size * 16
///   ensure memory is large enough via memory.grow
///   return v128 descriptor: [index=0, length=size, ptr, 0]
fn generate_buffer_alloc_fn(module: &mut walrus::Module, memory: MemoryId, buffer_alloc_ptr: GlobalId) -> FunctionId {
	let params = [ValType::I32];
	let results = [ValType::V128];
	let args: Vec<LocalId> = params.iter().map(|p| module.locals.add(*p)).collect();
	let mut builder = FunctionBuilder::new(&mut module.types, &params, &results);
	builder.name("buffer_alloc".to_string());
	let ptr = module.locals.add(ValType::I32);
	let end_addr = module.locals.add(ValType::I32);
	let mut b = builder.func_body();

	// ptr = buffer_alloc_ptr (current allocation pointer)
	b.global_get(buffer_alloc_ptr);
	b.local_set(ptr);

	// end_addr = ptr + size * 16
	b.local_get(ptr);
	b.local_get(args[0]);
	b.i32_const(4);
	b.binop(BinaryOp::I32Shl);
	b.binop(BinaryOp::I32Add);
	b.local_tee(end_addr);

	// buffer_alloc_ptr = end_addr
	b.global_set(buffer_alloc_ptr);

	// Ensure memory covers end_addr: grow if needed
	// required_pages = (end_addr + 65535) / 65536
	// current_pages = memory.size
	// if required_pages > current_pages: memory.grow(required_pages - current_pages)
	b.local_get(end_addr);
	b.i32_const(65535);
	b.binop(BinaryOp::I32Add);
	b.i32_const(16);
	b.binop(BinaryOp::I32ShrU);
	b.memory_size(memory);
	b.binop(BinaryOp::I32Sub);
	// Stack: pages_to_grow (may be <= 0)
	let pages_to_grow = module.locals.add(ValType::I32);
	b.local_tee(pages_to_grow);
	b.i32_const(0);
	b.binop(BinaryOp::I32GtS);
	b.if_else(
		None,
		|then| {
			then.local_get(pages_to_grow);
			then.memory_grow(memory);
			then.drop();
		},
		|_else| {},
	);

	// Build V128 descriptor: [0, size, ptr, 0] as i32x4
	// Start with zero vector
	b.i32_const(0);
	b.unop(UnaryOp::I32x4Splat);
	// Lane 0 = 0 (index, already zero)
	// Lane 1 = size
	b.local_get(args[0]);
	b.binop(BinaryOp::I32x4ReplaceLane { idx: 1 });
	// Lane 2 = ptr
	b.local_get(ptr);
	b.binop(BinaryOp::I32x4ReplaceLane { idx: 2 });

	builder.finish(args, &mut module.funcs)
}

struct MathImports {
	atan2: FunctionId,
	cos: FunctionId,
	exp2: FunctionId,
	log2: FunctionId,
	pow: FunctionId,
	sin: FunctionId,
	sincos: FunctionId,
	tan: FunctionId,
}

struct WasmGenerator<'ir> {
	program: &'ir ir::Program,
	module: RefCell<walrus::Module>,
	memory: MemoryId,
	sample_rate: GlobalId,
	state_ptr: GlobalId,
	buffer_alloc_ptr: GlobalId,
	track_command_ptr: GlobalId,
	track_index: GlobalId,
	note_alloc_ptr: GlobalId,
	note_ptr: GlobalId,
	prev_link_addr: GlobalId,
	math: MathImports,
	folded_multiply_fn: FunctionId,
	buffer_alloc_fn: FunctionId,
	gmdls_sample_fn: FunctionId,

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

		let set_parameter_function_id = self.generate_set_parameter_function();
		self.module().exports.add("set_parameter", set_parameter_function_id);

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
		let mut b = builder.func_body();

		let mut stack = args.clone();
		let mut pc = 0;
		self.generate_function_for_procedure_inner(
			&procedure.code, &mut pc, &mut b, &mut stack, &callees,
		);

		// Return values on stack
		for local in &stack {
			b.local_get(*local);
		}

		// Finish function
		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_function_for_procedure_inner(
		&self,
		code: &[ir::Instruction],
		pc: &mut usize,
		b: &mut InstrSeqBuilder,
		stack: &mut Vec<LocalId>,
		callees: &HashMap<u16, FunctionId>,
	) {
		// Helper: pop local from operand stack and push to wasm stack
		macro_rules! pop {
			() => {{
				b.local_get(stack.pop().unwrap());
			}}
		}

		// Helper: save wasm stack top into a new operand stack local
		macro_rules! push {
			() => {{
				let local = self.module().locals.add(ValType::V128);
				stack.push(local);
				b.local_set(local);
			}};
		}

		// Helper: pop N, run action, push M — only use where it covers the whole instruction
		macro_rules! op {
			($pops:expr, $pushes:expr, $action:expr) => {{
				for _ in 0..$pops {
					pop!();
				}
				$action;
				for _ in 0..$pushes {
					push!();
				}
			}};
		}

		// Save stack snapshot for reconciliation
		let stack_snapshot = stack.clone();

		let mut cell_stack = Vec::new();
		while *pc < code.len() {
			use ir::Instruction::*;
			match code[*pc] {
				Expand(_) => {},
				StackLoad(index) => {
					let index = stack.len() - 1 - (index as usize);
					let local = stack[index];
					stack.push(local);
				},
				StackStore(index) => {
					let local = stack.pop().unwrap();
					let index = stack.len() - 1 - (index as usize);
					stack[index] = local;
				},
				Pop => {
					stack.pop();
				},
				PopNext => {
					let local = stack.pop().unwrap();
					stack.pop();
					stack.push(local);
				},

				Call(proc_id, ..) => {
					let procedure = &self.program.procedures[proc_id as usize];
					let id = callees[&proc_id];

					let split_point = stack.len() - procedure.inputs.len();
					let inputs = stack.split_off(split_point);
					for input in inputs {
						b.local_get(input);
					}
					b.call(id);
					let outputs = procedure.outputs.iter().map(|_| self.module().locals.add(ValType::V128)).collect::<Vec<_>>();
					for output in outputs.iter().rev() {
						b.local_set(*output);
					}
					stack.extend(outputs);
				},

				Add => op!(2, 1, b.binop(BinaryOp::F64x2Add)),
				Sub => op!(2, 1, b.binop(BinaryOp::F64x2Sub)),
				Mul => op!(2, 1, b.binop(BinaryOp::F64x2Mul)),
				Div => op!(2, 1, b.binop(BinaryOp::F64x2Div)),
				And => op!(2, 1, b.binop(BinaryOp::V128And)),
				Or => op!(2, 1, b.binop(BinaryOp::V128Or)),
				Xor => op!(2, 1, b.binop(BinaryOp::V128Xor)),
				Min => op!(2, 1, b.binop(BinaryOp::F64x2Min)),
				Max => op!(2, 1, b.binop(BinaryOp::F64x2Max)),
				Eq => op!(2, 1, b.binop(BinaryOp::F64x2Eq)),
				Greater => op!(2, 1, b.binop(BinaryOp::F64x2Gt)),
				GreaterEq => op!(2, 1, b.binop(BinaryOp::F64x2Ge)),
				Less => op!(2, 1, b.binop(BinaryOp::F64x2Lt)),
				LessEq => op!(2, 1, b.binop(BinaryOp::F64x2Le)),
				Neq => op!(2, 1, b.binop(BinaryOp::F64x2Ne)),
				Ceil => op!(1, 1, b.unop(UnaryOp::F64x2Ceil)),
				Floor => op!(1, 1, b.unop(UnaryOp::F64x2Floor)),
				Round => op!(1, 1, b.unop(UnaryOp::F64x2Nearest)),
				Trunc => op!(1, 1, b.unop(UnaryOp::F64x2Trunc)),
				Sqrt => op!(1, 1, b.unop(UnaryOp::F64x2Sqrt)),

				Constant(value) => op!(0, 1, {
					b.f32_const(f32::from_bits(value));
					b.unop(UnaryOp::F64PromoteF32);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Parameter(index) => op!(0, 1, {
					b.i32_const(PARAMETER_ADDRESS + (index as i32) * 4);
					b.load(self.memory, LoadKind::F32, MemArg { align: 4, offset: 0 });
					b.unop(UnaryOp::F64PromoteF32);
					b.unop(UnaryOp::F64x2Splat);
				}),
				SampleRate => op!(0, 1, {
					b.global_get(self.sample_rate);
					b.unop(UnaryOp::F64PromoteF32);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Left => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64x2Splat);
				}),
				Right => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 1 });
					b.unop(UnaryOp::F64x2Splat);
				}),
				MergeLR => op!(2, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.binop(BinaryOp::F64x2ReplaceLane { idx: 1 });
				}),

				AndNot => {
					let s0 = stack.pop().unwrap();
					let s1 = stack.pop().unwrap();
					b.local_get(s1);
					b.local_get(s0);
					b.binop(BinaryOp::V128AndNot);
					push!();
				},
				AddSub => {
					let s0 = stack.pop().unwrap();
					let s1 = stack.pop().unwrap();
					b.local_get(s0);
					b.local_get(s1);
					b.local_get(s1);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Neg);
					b.binop(BinaryOp::F64x2ReplaceLane { idx: 0 });
					b.binop(BinaryOp::F64x2Add);
					push!();
				},
				SplitRL => {
					let loc = stack.pop().unwrap();
					b.local_get(loc);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64x2Splat);
					push!();
					b.local_get(loc);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 1 });
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				Cos => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.cos);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Exp2 => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.exp2);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Log2 => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.log2);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Sin => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.sin);
					b.unop(UnaryOp::F64x2Splat);
				}),
				Tan => op!(1, 1, {
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.tan);
					b.unop(UnaryOp::F64x2Splat);
				}),

				Atan2 => {
					let top = stack.pop().unwrap();
					let second = stack.pop().unwrap();
					b.local_get(top);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.local_get(second);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.atan2);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},
				Pow => {
					let top = stack.pop().unwrap();
					let second = stack.pop().unwrap();
					b.local_get(top);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.local_get(second);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.pow);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},
				SinCos => {
					let input = stack.pop().unwrap();
					let cos_tmp = self.module().locals.add(ValType::F64);
					b.local_get(input);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.call(self.math.sincos);
					b.local_set(cos_tmp);
					b.unop(UnaryOp::F64x2Splat);
					push!();
					b.local_get(cos_tmp);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				CellInit => {
					b.global_get(self.state_ptr);
					b.global_get(self.state_ptr);
					b.i32_const(16);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.state_ptr);
					pop!();
					b.store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });
				},
				CellRead => {
					b.global_get(self.state_ptr);
					b.global_get(self.state_ptr);
					b.i32_const(16);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.state_ptr);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push!();
				},
				CellPush => {
					let state_addr = self.module().locals.add(ValType::I32);
					cell_stack.push(state_addr);
					b.global_get(self.state_ptr);
					b.local_tee(state_addr);
					b.global_get(self.state_ptr);
					b.i32_const(16);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.state_ptr);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push!();
				},
				CellFetch => {
					let state_addr = *cell_stack.last().unwrap();
					b.local_get(state_addr);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push!();
				},
				CellPop => {
					let state_addr = cell_stack.pop().unwrap();
					b.local_get(state_addr);
					pop!();
					b.store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });
				},

				StateEnter => {
					let saved_state = self.module().locals.add(ValType::I32);
					cell_stack.push(saved_state);
					b.global_get(self.state_ptr);
					b.local_set(saved_state);
				},
				StateLeave => {
					let saved_state = cell_stack.pop().unwrap();
					b.local_get(saved_state);
					b.global_set(self.state_ptr);
				},

				BufferAlloc(_width) => {
					// Pop size (V128), extract lane 0 as f64, convert to i32
					let size_v128 = stack.pop().unwrap();
					b.local_get(size_v128);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);
					// Call buffer_alloc(size) -> v128 descriptor
					b.call(self.buffer_alloc_fn);
					push!();
				},

				BufferLoad => {
					// Pop descriptor (V128 with i32x4 layout: [index, length, ptr, 0])
					let desc = stack.pop().unwrap();
					// addr = ptr + index * 16
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 2 }); // ptr
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 0 }); // index
					b.i32_const(4);
					b.binop(BinaryOp::I32Shl);
					b.binop(BinaryOp::I32Add);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push!();
				},

				BufferLoadWithOffset => {
					// Pop offset (V128) and descriptor (V128)
					let offset_v128 = stack.pop().unwrap();
					let desc = stack.pop().unwrap();

					// Convert offset to i32 (round-to-nearest, like cvtsd2si)
					let offset_i32 = self.module().locals.add(ValType::I32);
					b.local_get(offset_v128);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);
					b.local_set(offset_i32);

					// read_index = index - offset
					let read_index = self.module().locals.add(ValType::I32);
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 0 });
					b.local_get(offset_i32);
					b.binop(BinaryOp::I32Sub);
					b.local_set(read_index);

					// if read_index < 0, wrap: read_index += length
					b.local_get(read_index);
					b.i32_const(0);
					b.binop(BinaryOp::I32LtS);
					b.if_else(
						None,
						|then| {
							then.local_get(read_index);
							then.local_get(desc);
							then.unop(UnaryOp::I32x4ExtractLane { idx: 1 });
							then.binop(BinaryOp::I32Add);
							then.local_set(read_index);
						},
						|_else| {},
					);

					// addr = ptr + read_index * 16
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 2 });
					b.local_get(read_index);
					b.i32_const(4);
					b.binop(BinaryOp::I32Shl);
					b.binop(BinaryOp::I32Add);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					push!();
				},

				BufferLoadIndexed => {
					// Pop index (V128) and descriptor (V128)
					let index_v128 = stack.pop().unwrap();
					let desc = stack.pop().unwrap();

					// Convert index to i32
					let idx = self.module().locals.add(ValType::I32);
					b.local_get(index_v128);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);
					b.local_set(idx);

					// Bounds check: if index >= length, return zero
					let result = self.module().locals.add(ValType::V128);
					// Default to zero
					b.i32_const(0);
					b.unop(UnaryOp::I32x4Splat);
					b.local_set(result);

					b.local_get(idx);
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 1 }); // length
					b.binop(BinaryOp::I32LtU);
					b.if_else(
						None,
						|then| {
							// addr = ptr + index * 16
							then.local_get(desc);
							then.unop(UnaryOp::I32x4ExtractLane { idx: 2 }); // ptr
							then.local_get(idx);
							then.i32_const(4);
							then.binop(BinaryOp::I32Shl);
							then.binop(BinaryOp::I32Add);
							then.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
							then.local_set(result);
						},
						|_else| {},
					);

					b.local_get(result);
					push!();
				},

				BufferStoreAndStep => {
					// Pop value (V128) and descriptor (V128)
					let value = stack.pop().unwrap();
					let desc = stack.pop().unwrap();

					// Store value at ptr + index * 16
					let index = self.module().locals.add(ValType::I32);
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 0 }); // index
					b.local_set(index);

					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 2 }); // ptr
					b.local_get(index);
					b.i32_const(4);
					b.binop(BinaryOp::I32Shl);
					b.binop(BinaryOp::I32Add);
					b.local_get(value);
					b.store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });

					// Increment index
					b.local_get(index);
					b.i32_const(1);
					b.binop(BinaryOp::I32Add);
					b.local_set(index);

					// Wrap: if index >= length, index = 0
					b.local_get(index);
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 1 }); // length
					b.binop(BinaryOp::I32GeU);
					b.if_else(
						None,
						|then| {
							then.i32_const(0);
							then.local_set(index);
						},
						|_else| {},
					);

					// Update descriptor with new index
					b.local_get(desc);
					b.local_get(index);
					b.binop(BinaryOp::I32x4ReplaceLane { idx: 0 });
					push!();
				},

				BufferIndex => {
					// Pop descriptor, push current index as f64x2
					let desc = stack.pop().unwrap();
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 0 }); // index
					b.unop(UnaryOp::F64ConvertSI32);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				BufferLength => {
					// Pop descriptor, push length as f64x2
					let desc = stack.pop().unwrap();
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 1 }); // length
					b.unop(UnaryOp::F64ConvertSI32);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				Random => {
					// Pop top (seed a) and second (seed b)
					let top = stack.pop().unwrap();
					let second = stack.pop().unwrap();

					// Extract lane 0 of second arg, reinterpret as i64 bits
					let bits = self.module().locals.add(ValType::I64);
					b.local_get(second);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::I64ReinterpretF64);
					b.local_set(bits);

					// Convert top arg lane 0 to i32 (cvtsd2si equivalent)
					b.local_get(top);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);

					// First folded multiply
					b.call(self.folded_multiply_fn);
					// XOR with upper 32 bits of second arg
					b.local_get(bits);
					b.i64_const(32);
					b.binop(BinaryOp::I64ShrU);
					b.unop(UnaryOp::I32WrapI64);
					b.binop(BinaryOp::I32Xor);

					// Second folded multiply
					b.call(self.folded_multiply_fn);
					// XOR with lower 32 bits of second arg
					b.local_get(bits);
					b.unop(UnaryOp::I32WrapI64);
					b.binop(BinaryOp::I32Xor);

					// Third folded multiply
					b.call(self.folded_multiply_fn);

					// Convert signed i32 to f64, splat to V128
					b.unop(UnaryOp::F64ConvertSI32);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				RepeatInit => {
					// Pop limit, store in cell (CellInit pattern), push limit + counter=0
					let limit_local = stack.pop().unwrap();

					// CellInit: store limit at state_ptr, advance state_ptr
					b.global_get(self.state_ptr);
					b.global_get(self.state_ptr);
					b.i32_const(16);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.state_ptr);
					b.local_get(limit_local);
					b.store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });

					// Push limit and counter(=0) onto operand stack
					stack.push(limit_local);
					let counter_local = self.module().locals.add(ValType::V128);
					b.f64_const(0.0);
					b.unop(UnaryOp::F64x2Splat);
					b.local_set(counter_local);
					stack.push(counter_local);

					*pc += 1;
					b.loop_(None, |loop_b| {
						self.generate_function_for_procedure_inner(
							code, pc, loop_b, stack, callees,
						);
					});

					stack.pop(); // counter
					stack.pop(); // limit
					continue;
				},

				RepeatStart => {
					// CellRead: read limit from state memory, advance state_ptr
					let limit_local = self.module().locals.add(ValType::V128);
					b.global_get(self.state_ptr);
					b.global_get(self.state_ptr);
					b.i32_const(16);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.state_ptr);
					b.load(self.memory, LoadKind::V128, MemArg { align: 16, offset: 0 });
					b.local_set(limit_local);

					// Push limit and counter(=0) onto operand stack
					stack.push(limit_local);
					let counter_local = self.module().locals.add(ValType::V128);
					b.f64_const(0.0);
					b.unop(UnaryOp::F64x2Splat);
					b.local_set(counter_local);
					stack.push(counter_local);

					*pc += 1;
					b.loop_(None, |loop_b| {
						self.generate_function_for_procedure_inner(
							code, pc, loop_b, stack, callees,
						);
					});

					stack.pop(); // counter
					stack.pop(); // limit
					continue;
				},

				RepeatEnd => {
					let snapshot = stack_snapshot.clone();

					// Pop counter and limit from operand stack
					let counter = stack.pop().unwrap();
					let _limit = stack.pop().unwrap();

					// Reconcile: copy changed locals back to their snapshot positions
					let snapshot_base_len = snapshot.len() - 2;
					for i in 0..stack.len().min(snapshot_base_len) {
						if stack[i] != snapshot[i] {
							b.local_get(stack[i]);
							b.local_set(snapshot[i]);
						}
					}

					// Get the snapshot's counter and limit locals
					let snapshot_counter = snapshot[snapshot.len() - 1];
					let snapshot_limit = snapshot[snapshot.len() - 2];

					// Increment counter: load, extract lane 0, add 1.0, splat, store
					b.local_get(counter);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.f64_const(1.0);
					b.binop(BinaryOp::F64Add);
					b.unop(UnaryOp::F64x2Splat);
					b.local_set(snapshot_counter);

					// Compare counter < limit
					b.local_get(snapshot_counter);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.local_get(snapshot_limit);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.binop(BinaryOp::F64Lt);

					// Branch back to loop start if counter < limit
					b.br_if(b.id());

					// After loop: restore stack to snapshot minus counter and limit
					*stack = stack_snapshot;

					*pc += 1;
					return;
				},

				BufferInitStart => {
					// Descriptor is on top of operand stack.
					// Set up a wasm loop. The body will generate a value on top.
					*pc += 1;
					b.loop_(None, |loop_b| {
						self.generate_function_for_procedure_inner(
							code, pc, loop_b, stack, callees,
						);
					});
					// After loop: descriptor remains on stack
					continue;
				},

				BufferInitEnd => {
					let snapshot = stack_snapshot.clone();

					// Pop value (generated by body) and descriptor
					let value = stack.pop().unwrap();
					let desc = stack.pop().unwrap();

					// --- Store value at ptr + index * 16 ---
					let index = self.module().locals.add(ValType::I32);
					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 0 }); // index
					b.local_set(index);

					b.local_get(desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 2 }); // ptr
					b.local_get(index);
					b.i32_const(4);
					b.binop(BinaryOp::I32Shl);
					b.binop(BinaryOp::I32Add);
					b.local_get(value);
					b.store(self.memory, StoreKind::V128, MemArg { align: 16, offset: 0 });

					// Increment index
					b.local_get(index);
					b.i32_const(1);
					b.binop(BinaryOp::I32Add);
					b.local_set(index);

					// Update descriptor with new index
					let snapshot_desc = snapshot[snapshot.len() - 1];
					b.local_get(desc);
					b.local_get(index);
					b.binop(BinaryOp::I32x4ReplaceLane { idx: 0 });
					b.local_set(snapshot_desc);

					// Reconcile other changed locals back to snapshot positions
					let snapshot_base_len = snapshot.len() - 1; // exclude descriptor
					for i in 0..stack.len().min(snapshot_base_len) {
						if stack[i] != snapshot[i] {
							b.local_get(stack[i]);
							b.local_set(snapshot[i]);
						}
					}

					// Compare index < length, branch back to loop start if true
					b.local_get(index);
					b.local_get(snapshot_desc);
					b.unop(UnaryOp::I32x4ExtractLane { idx: 1 }); // length
					b.binop(BinaryOp::I32LtU);
					b.br_if(b.id());

					// After loop: restore stack to snapshot (descriptor with final index)
					*stack = stack_snapshot;

					*pc += 1;
					return;
				},

				GmDlsSample => {
					// Pop index (top), pop sound_id (second)
					let index_local = stack.pop().unwrap();
					let sound_id_local = stack.pop().unwrap();
					// Extract sound_id as i32
					b.local_get(sound_id_local);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);
					// Extract index as i32
					b.local_get(index_local);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.unop(UnaryOp::F64Nearest);
					b.unop(UnaryOp::I32TruncSSatF64);
					// Call gmdls_sample(sound_id, index) -> i32
					b.call(self.gmdls_sample_fn);
					// Convert signed i32 to f64, splat to v128
					b.unop(UnaryOp::F64ConvertSI32);
					b.unop(UnaryOp::F64x2Splat);
					push!();
				},

				IfGreaterEq => {
					// Compare: top >= second-from-top (both f64x2, compare lane 0)
					let b_val = stack[stack.len() - 1]; // top
					let a_val = stack[stack.len() - 2]; // second

					b.local_get(b_val);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.local_get(a_val);
					b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
					b.binop(BinaryOp::F64Ge);

					*pc += 1;
					let pc_cell = Cell::new(*pc);
					let saved_stack = stack.clone();
					let stack_cell = RefCell::new(stack.clone());

					b.if_else(
						None,
						|then_b| {
							let mut then_pc = pc_cell.get();
							self.generate_function_for_procedure_inner(
								code, &mut then_pc, then_b, &mut stack_cell.borrow_mut(), callees,
							);
							pc_cell.set(then_pc);
						},
						|else_b| {
							// Reset stack to same input state for else-branch
							*stack_cell.borrow_mut() = saved_stack;
							let mut else_pc = pc_cell.get();
							if let [Else, ..] = code[else_pc..] {
								else_pc += 1;
								self.generate_function_for_procedure_inner(
									code, &mut else_pc, else_b, &mut stack_cell.borrow_mut(), callees,
								);
								pc_cell.set(else_pc);
							}
						},
					);

					*stack = stack_cell.into_inner();
					*pc = pc_cell.get();
					// pc points at EndIf; fall through to *pc += 1 to advance past it
				},

				Else => {
					// Reached from recursive call inside then-branch of IfGreaterEq
					return;
				},

				EndIf => {
					// Reached from recursive call inside then or else branch
					return;
				},

				PlayInstrument(static_proc_id, dynamic_proc_id) => {
					let static_proc = &self.program.procedures[static_proc_id as usize];
					let dynamic_proc = &self.program.procedures[dynamic_proc_id as usize];
					let static_fn = callees[&static_proc_id];
					let dynamic_fn = callees[&dynamic_proc_id];

					let saved_state = self.module().locals.add(ValType::I32);
					let current_track = self.module().locals.add(ValType::I32);
					let cmd_ptr = self.module().locals.add(ValType::I32);
					let cmd_type = self.module().locals.add(ValType::I32);
					let cmd_key = self.module().locals.add(ValType::I32);
					let cmd_velocity = self.module().locals.add(ValType::I32);
					let note_addr = self.module().locals.add(ValType::I32);
					let link_addr = self.module().locals.add(ValType::I32);

					// Save state_ptr
					b.global_get(self.state_ptr);
					b.local_set(saved_state);

					// current_track = track_index
					b.global_get(self.track_index);
					b.local_set(current_track);

					// --- Phase 1: Process note commands for this track ---
					b.block(None, |done_cmds| {
						let done_cmds_id = done_cmds.id();
						done_cmds.loop_(None, |cmd_loop| {
							let cmd_loop_id = cmd_loop.id();

							cmd_loop.global_get(self.track_command_ptr);
							cmd_loop.local_set(cmd_ptr);

							// Read command track; if != current_track, break
							cmd_loop.local_get(cmd_ptr);
							cmd_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
							cmd_loop.local_get(current_track);
							cmd_loop.binop(BinaryOp::I32Ne);
							cmd_loop.br_if(done_cmds_id);

							// Read type, key, velocity
							cmd_loop.local_get(cmd_ptr);
							cmd_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
							cmd_loop.local_set(cmd_type);
							cmd_loop.local_get(cmd_ptr);
							cmd_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });
							cmd_loop.local_set(cmd_key);
							cmd_loop.local_get(cmd_ptr);
							cmd_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 12 });
							cmd_loop.local_set(cmd_velocity);

							// Advance track_command_ptr
							cmd_loop.local_get(cmd_ptr);
							cmd_loop.i32_const(16);
							cmd_loop.binop(BinaryOp::I32Add);
							cmd_loop.global_set(self.track_command_ptr);

							cmd_loop.local_get(cmd_type);
							cmd_loop.if_else(
								None,
								|on_b| {
									// --- ON command ---
									on_b.global_get(self.note_alloc_ptr);
									on_b.local_set(note_addr);

									// Write header: next = old list head
									on_b.local_get(note_addr);
									on_b.i32_const(NOTE_LISTS_ADDRESS);
									on_b.local_get(current_track);
									on_b.i32_const(4);
									on_b.binop(BinaryOp::I32Mul);
									on_b.binop(BinaryOp::I32Add);
									on_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
									on_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });

									// gate = 0x7FFFFFFF
									on_b.local_get(note_addr);
									on_b.i32_const(0x7FFFFFFF);
									on_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });

									// key
									on_b.local_get(note_addr);
									on_b.local_get(cmd_key);
									on_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });

									// velocity
									on_b.local_get(note_addr);
									on_b.local_get(cmd_velocity);
									on_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 12 });

									// Update list head
									on_b.i32_const(NOTE_LISTS_ADDRESS);
									on_b.local_get(current_track);
									on_b.i32_const(4);
									on_b.binop(BinaryOp::I32Mul);
									on_b.binop(BinaryOp::I32Add);
									on_b.local_get(note_addr);
									on_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });

									// Set note_ptr for ReadNoteProperty and Kill
									on_b.local_get(note_addr);
									on_b.global_set(self.note_ptr);

									// state_ptr = note_addr + 16
									on_b.local_get(note_addr);
									on_b.i32_const(16);
									on_b.binop(BinaryOp::I32Add);
									on_b.global_set(self.state_ptr);

									// Call static proc
									let n_inputs = static_proc.inputs.len();
									for i in 0..n_inputs {
										let idx = stack.len() - n_inputs + i;
										on_b.local_get(stack[idx]);
									}
									on_b.call(static_fn);
									let n_outputs = static_proc.outputs.len();
									for i in (0..n_outputs).rev() {
										let idx = stack.len() - n_outputs + i;
										on_b.local_set(stack[idx]);
									}

									// note_alloc_ptr = state_ptr
									on_b.global_get(self.state_ptr);
									on_b.global_set(self.note_alloc_ptr);
								},
								|off_b| {
									// --- OFF command ---
									// Walk linked list, find note with matching key and gate > 0, set gate = 0
									off_b.i32_const(NOTE_LISTS_ADDRESS);
									off_b.local_get(current_track);
									off_b.i32_const(4);
									off_b.binop(BinaryOp::I32Mul);
									off_b.binop(BinaryOp::I32Add);
									off_b.local_set(link_addr);

									off_b.block(None, |found_b| {
										let found_id = found_b.id();
										found_b.loop_(None, |search_b| {
											let search_id = search_b.id();

											search_b.local_get(link_addr);
											search_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
											search_b.local_set(note_addr);

											// if note_addr == 0, not found
											search_b.local_get(note_addr);
											search_b.i32_const(0);
											search_b.binop(BinaryOp::I32Eq);
											search_b.br_if(found_id);

											// Check key match and gate > 0
											search_b.local_get(note_addr);
											search_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });
											search_b.local_get(cmd_key);
											search_b.binop(BinaryOp::I32Eq);
											search_b.if_else(
												None,
												|match_b| {
													match_b.local_get(note_addr);
													match_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
													match_b.i32_const(0);
													match_b.binop(BinaryOp::I32GtS);
													match_b.if_else(
														None,
														|set_b| {
															set_b.local_get(note_addr);
															set_b.i32_const(0);
															set_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
															set_b.br(found_id);
														},
														|_| {},
													);
												},
												|_| {},
											);

											// Move to next note
											search_b.local_get(note_addr);
											search_b.local_set(link_addr);
											search_b.br(search_id);
										});
									});
								},
							);

							cmd_loop.br(cmd_loop_id);
						});
					});

					// --- Phase 2: Process active notes ---
					b.i32_const(NOTE_LISTS_ADDRESS);
					b.local_get(current_track);
					b.i32_const(4);
					b.binop(BinaryOp::I32Mul);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.prev_link_addr);

					b.block(None, |done_notes| {
						let done_notes_id = done_notes.id();
						done_notes.loop_(None, |note_loop| {
							let note_loop_id = note_loop.id();

							note_loop.global_get(self.prev_link_addr);
							note_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
							note_loop.local_set(note_addr);

							// if note_addr == 0, done
							note_loop.local_get(note_addr);
							note_loop.i32_const(0);
							note_loop.binop(BinaryOp::I32Eq);
							note_loop.br_if(done_notes_id);

							// Set note_ptr for ReadNoteProperty and Kill
							note_loop.local_get(note_addr);
							note_loop.global_set(self.note_ptr);

							// state_ptr = note_addr + 16
							note_loop.local_get(note_addr);
							note_loop.i32_const(16);
							note_loop.binop(BinaryOp::I32Add);
							note_loop.global_set(self.state_ptr);

							// Call dynamic proc
							let n_inputs = dynamic_proc.inputs.len();
							for i in 0..n_inputs {
								let idx = stack.len() - n_inputs + i;
								note_loop.local_get(stack[idx]);
							}
							note_loop.call(dynamic_fn);
							let n_outputs = dynamic_proc.outputs.len();
							for i in (0..n_outputs).rev() {
								let idx = stack.len() - n_outputs + i;
								note_loop.local_set(stack[idx]);
							}

							// Decrement gate
							note_loop.local_get(note_addr);
							note_loop.local_get(note_addr);
							note_loop.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
							note_loop.i32_const(1);
							note_loop.binop(BinaryOp::I32Sub);
							note_loop.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });

							// Advance prev_link_addr
							note_loop.local_get(note_addr);
							note_loop.global_set(self.prev_link_addr);

							note_loop.br(note_loop_id);
						});
					});

					// Restore state, increment track_index
					b.local_get(saved_state);
					b.global_set(self.state_ptr);
					b.global_get(self.track_index);
					b.i32_const(1);
					b.binop(BinaryOp::I32Add);
					b.global_set(self.track_index);
				},

				Kill => {
					// Remove current note from linked list
					b.global_get(self.prev_link_addr);
					b.global_get(self.note_ptr);
					b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
					b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
				},

				ReadNoteProperty(property) => {
					match property {
						ir::NoteProperty::Gate => {
							// Must produce a bitmask (all-1s/all-0s) for use with And/AndNot/Or select pattern.
							// Push 0.0, then gate value, then F64x2Gt to get proper bitmask.
							b.f64_const(0.0);
							b.unop(UnaryOp::F64x2Splat);
							push!();
							b.global_get(self.note_ptr);
							b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
							b.unop(UnaryOp::F64ConvertSI32);
							b.unop(UnaryOp::F64x2Splat);
							push!();
							// gate > 0 produces bitmask
							op!(2, 1, b.binop(BinaryOp::F64x2Gt));
						},
						ir::NoteProperty::Key => {
							b.global_get(self.note_ptr);
							b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });
							b.unop(UnaryOp::F64ConvertSI32);
							b.unop(UnaryOp::F64x2Splat);
							push!();
						},
						ir::NoteProperty::Velocity => {
							b.global_get(self.note_ptr);
							b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 12 });
							b.unop(UnaryOp::F64ConvertSI32);
							b.unop(UnaryOp::F64x2Splat);
							push!();
						},
					}
				},
			}
			*pc += 1;
		}
	}

	fn generate_initialize_function(&mut self) -> FunctionId {
		let params = [ValType::F32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		let mut b = builder.func_body();

		b.i32_const(STATE_ADDRESS);
		b.global_set(self.state_ptr);
		b.i32_const(BUFFER_BASE_ADDRESS);
		b.global_set(self.buffer_alloc_ptr);
		b.i32_const(NOTE_COMMANDS_ADDRESS);
		b.global_set(self.track_command_ptr);

		// Clear note list heads
		let num_tracks = self.program.track_order.len();
		if num_tracks > 0 {
			let ptr = self.module().locals.add(ValType::I32);
			let end = NOTE_LISTS_ADDRESS + (num_tracks as i32) * 4;
			b.i32_const(NOTE_LISTS_ADDRESS);
			b.local_set(ptr);
			b.loop_(None, |loop_b| {
				let loop_id = loop_b.id();
				loop_b.local_get(ptr);
				loop_b.i32_const(0);
				loop_b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
				loop_b.local_get(ptr);
				loop_b.i32_const(4);
				loop_b.binop(BinaryOp::I32Add);
				loop_b.local_set(ptr);
				loop_b.local_get(ptr);
				loop_b.i32_const(end);
				loop_b.binop(BinaryOp::I32LtU);
				loop_b.br_if(loop_id);
			});
		}

		// Write command sentinel
		b.global_get(self.track_command_ptr);
		b.i32_const(COMMAND_SENTINEL);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });

		b.call(self.get_function_id(self.program.main_static_proc_id as u16));

		// Allocate state for notes where state allocation for main left off
		b.global_get(self.state_ptr);
		b.global_set(self.note_alloc_ptr);

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_note_on_function(&mut self) -> FunctionId {
		// note_on(track: i32, key: i32, velocity: i32)
		// Writes an ON command (type=1) sorted by (track, type) via bubble insertion.
		let params = [ValType::I32, ValType::I32, ValType::I32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		builder.name("note_on".to_string());
		let insert_ptr = self.module().locals.add(ValType::I32);
		let sort_key = self.module().locals.add(ValType::I32);
		let prev_sort_key = self.module().locals.add(ValType::I32);
		let mut b = builder.func_body();

		let note_cmds_addr = NOTE_COMMANDS_ADDRESS;

		b.global_get(self.track_command_ptr);
		b.local_set(insert_ptr);

		// sort_key = track * 2 + 1 (ON)
		b.local_get(args[0]);
		b.i32_const(2);
		b.binop(BinaryOp::I32Mul);
		b.i32_const(1);
		b.binop(BinaryOp::I32Add);
		b.local_set(sort_key);

		b.block(None, |done| {
			let done_id = done.id();
			done.loop_(None, |loop_b| {
				let loop_id = loop_b.id();
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(note_cmds_addr);
				loop_b.binop(BinaryOp::I32LeS);
				loop_b.br_if(done_id);

				// prev_sort_key = prev_track * 2 + prev_type
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
				loop_b.i32_const(2);
				loop_b.binop(BinaryOp::I32Mul);
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
				loop_b.binop(BinaryOp::I32Add);
				loop_b.local_set(prev_sort_key);

				loop_b.local_get(prev_sort_key);
				loop_b.local_get(sort_key);
				loop_b.binop(BinaryOp::I32LeS);
				loop_b.br_if(done_id);

				// Copy 16 bytes forward
				loop_b.local_get(insert_ptr);
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::V128, MemArg { align: 4, offset: 0 });
				loop_b.store(self.memory, StoreKind::V128, MemArg { align: 4, offset: 0 });

				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.local_set(insert_ptr);
				loop_b.br(loop_id);
			});
		});

		// Write command: [track, 1, key, velocity]
		b.local_get(insert_ptr);
		b.local_get(args[0]);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
		b.local_get(insert_ptr);
		b.i32_const(1);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
		b.local_get(insert_ptr);
		b.local_get(args[1]);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });
		b.local_get(insert_ptr);
		b.local_get(args[2]);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 12 });

		// track_command_ptr += 16
		b.global_get(self.track_command_ptr);
		b.i32_const(16);
		b.binop(BinaryOp::I32Add);
		b.global_set(self.track_command_ptr);

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_note_off_function(&mut self) -> FunctionId {
		// note_off(track: i32, key: i32)
		// Writes an OFF command (type=0) sorted by (track, type) via bubble insertion.
		let params = [ValType::I32, ValType::I32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		builder.name("note_off".to_string());
		let insert_ptr = self.module().locals.add(ValType::I32);
		let sort_key = self.module().locals.add(ValType::I32);
		let prev_sort_key = self.module().locals.add(ValType::I32);
		let mut b = builder.func_body();

		let note_cmds_addr = NOTE_COMMANDS_ADDRESS;

		b.global_get(self.track_command_ptr);
		b.local_set(insert_ptr);

		// sort_key = track * 2 (OFF = 0)
		b.local_get(args[0]);
		b.i32_const(2);
		b.binop(BinaryOp::I32Mul);
		b.local_set(sort_key);

		b.block(None, |done| {
			let done_id = done.id();
			done.loop_(None, |loop_b| {
				let loop_id = loop_b.id();
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(note_cmds_addr);
				loop_b.binop(BinaryOp::I32LeS);
				loop_b.br_if(done_id);

				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
				loop_b.i32_const(2);
				loop_b.binop(BinaryOp::I32Mul);
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
				loop_b.binop(BinaryOp::I32Add);
				loop_b.local_set(prev_sort_key);

				loop_b.local_get(prev_sort_key);
				loop_b.local_get(sort_key);
				loop_b.binop(BinaryOp::I32LeS);
				loop_b.br_if(done_id);

				loop_b.local_get(insert_ptr);
				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.load(self.memory, LoadKind::V128, MemArg { align: 4, offset: 0 });
				loop_b.store(self.memory, StoreKind::V128, MemArg { align: 4, offset: 0 });

				loop_b.local_get(insert_ptr);
				loop_b.i32_const(16);
				loop_b.binop(BinaryOp::I32Sub);
				loop_b.local_set(insert_ptr);
				loop_b.br(loop_id);
			});
		});

		// Write command: [track, 0, key, 0]
		b.local_get(insert_ptr);
		b.local_get(args[0]);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });
		b.local_get(insert_ptr);
		b.i32_const(0);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 4 });
		b.local_get(insert_ptr);
		b.local_get(args[1]);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 8 });
		b.local_get(insert_ptr);
		b.i32_const(0);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 12 });

		// track_command_ptr += 16
		b.global_get(self.track_command_ptr);
		b.i32_const(16);
		b.binop(BinaryOp::I32Add);
		b.global_set(self.track_command_ptr);

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_set_parameter_function(&mut self) -> FunctionId {
		// set_parameter(index: i32, value: f32)
		// Stores value at PARAMETER_ADDRESS + index * 4
		let params = [ValType::I32, ValType::F32];
		let results = [];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		let mut b = builder.func_body();

		// address = PARAMETER_ADDRESS + index * 4
		b.local_get(args[0]);
		b.i32_const(4);
		b.binop(BinaryOp::I32Mul);
		b.i32_const(PARAMETER_ADDRESS);
		b.binop(BinaryOp::I32Add);
		// store f32 value
		b.local_get(args[1]);
		b.store(self.memory, StoreKind::F32, MemArg { align: 4, offset: 0 });

		builder.finish(args, &mut self.module().funcs)
	}

	fn generate_next_sample_function(&mut self) -> FunctionId {
		let params = [];
		let results = [ValType::F64, ValType::F64];
		let args = params.iter().map(|param| self.module().locals.add(*param)).collect::<Vec<_>>();
		let mut builder = FunctionBuilder::new(&mut self.module().types, &params, &results);
		let mut b = builder.func_body();

		// Write sentinel at end of commands, then reset for reading
		b.global_get(self.track_command_ptr);
		b.i32_const(COMMAND_SENTINEL);
		b.store(self.memory, StoreKind::I32 { atomic: false }, MemArg { align: 4, offset: 0 });

		b.i32_const(STATE_ADDRESS);
		b.global_set(self.state_ptr);
		b.i32_const(0);
		b.global_set(self.track_index);
		b.i32_const(NOTE_COMMANDS_ADDRESS);
		b.global_set(self.track_command_ptr);

		b.call(self.get_function_id(self.program.main_dynamic_proc_id as u16));
		let result_local = self.module().locals.add(ValType::V128);
		b.local_tee(result_local);
		b.unop(UnaryOp::F64x2ExtractLane { idx: 0 });
		b.local_get(result_local);
		b.unop(UnaryOp::F64x2ExtractLane { idx: 1 });

		// Reset track_command_ptr for next batch
		b.i32_const(NOTE_COMMANDS_ADDRESS);
		b.global_set(self.track_command_ptr);

		builder.finish(args, &mut self.module().funcs)
	}
}

fn load_gmdls() -> Vec<u8> {
	let path = if cfg!(target_os = "windows") {
		let sys_dir = std::env::var("SystemRoot").unwrap_or_else(|_| "C:\\Windows".to_string());
		std::path::PathBuf::from(sys_dir).join("system32").join("drivers").join("gm.dls")
	} else {
		let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
		std::path::PathBuf::from(home).join(".jingler").join("gm.dls")
	};
	std::fs::read(&path).unwrap_or_default()
}

fn gmdls_lookup(data: &[u8], sound_id: i32, index: i32) -> i32 {
	if sound_id < 0 || sound_id as usize >= GMDLS_COUNT {
		return 0;
	}
	if data.len() < GMDLS_DATA {
		return 0;
	}

	let off_addr = GMDLS_OFFSETS + sound_id as usize * 4;
	if off_addr + 4 > data.len() {
		return 0;
	}
	let offset = u32::from_le_bytes(data[off_addr..off_addr + 4].try_into().unwrap()) as usize;
	let block = GMDLS_DATA + offset;
	if block + 4 > data.len() {
		return 0;
	}
	let wsmp_rel = u32::from_le_bytes(data[block..block + 4].try_into().unwrap()) as usize;
	let wsmp = block + wsmp_rel;
	if wsmp + 12 > data.len() || wsmp < 4 {
		return 0;
	}

	let loop_length = u32::from_le_bytes(data[wsmp..wsmp + 4].try_into().unwrap());
	let loop_base = u32::from_le_bytes(data[wsmp - 4..wsmp].try_into().unwrap());

	let mut eff_index = index as u32;
	if loop_length != 0 && eff_index >= loop_base {
		eff_index = (eff_index - loop_base) % loop_length + loop_base;
	}

	let data_length = u32::from_le_bytes(data[wsmp + 8..wsmp + 12].try_into().unwrap());
	let sample_count = data_length / 2;

	if eff_index >= sample_count {
		return 0;
	}

	let addr = wsmp + 10 + eff_index as usize * 2;
	if addr + 4 > data.len() {
		return 0;
	}
	i32::from_le_bytes(data[addr..addr + 4].try_into().unwrap())
}
