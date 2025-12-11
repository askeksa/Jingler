extern crate nasm_rs;

fn main() {
	if !std::env::var("CARGO_CFG_TARGET_ARCH").unwrap().starts_with("x86") {
		return;
	}

	let objects = nasm_rs::Build::new()
		.file("../../runtime/cmd.asm")
		.compile_objects()
		.unwrap();

	println!("cargo:rerun-if-changed=../../runtime/cmd.asm");
	println!("cargo:rerun-if-changed=../../runtime/jingler.asm");
	println!("cargo:rerun-if-changed=../../runtime/used_instructions.inc");

	for object in objects {
		println!("cargo:rustc-link-arg={}", object.display());
	}
}
