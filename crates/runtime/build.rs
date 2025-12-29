extern crate nasm_rs;

fn main() {
	if !std::env::var("CARGO_CFG_TARGET_ARCH").unwrap().starts_with("x86") {
		return;
	}

	let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
	let asm_path = std::path::Path::new(&manifest_dir).join("../../runtime/cmd.asm");

	if !asm_path.exists() {
		panic!("Assembly file not found at {:?}", asm_path);
	}

	let mut nasm = nasm_rs::Build::new();
	nasm.file(asm_path);
	let objects = nasm.compile_objects().expect("Failed to compile assembly objects");

	let mut cc = cc::Build::new();
	for object in objects {
		cc.object(object);
	}
	cc.compile("jingler_runtime");

	println!("cargo:rerun-if-changed=../../runtime/cmd.asm");
	println!("cargo:rerun-if-changed=../../runtime/jingler.asm");
	println!("cargo:rerun-if-changed=../../runtime/used_instructions.inc");
}
