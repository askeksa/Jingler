extern crate nasm_rs;

fn main() {
	#[cfg(not(target_env = "msvc"))]
	nasm_rs::compile_library("libjingler_cmd.a", &["../../runtime/cmd.asm"]);

	#[cfg(target_env = "msvc")]
	nasm_rs::compile_library("jingler_cmd.lib", &["../../runtime/cmd.asm"]);

	println!("cargo:rerun-if-changed=../../runtime/cmd.asm");
	println!("cargo:rerun-if-changed=../../runtime/jingler.asm");
	println!("cargo:rerun-if-changed=../../runtime/used_instructions.inc");

	println!("cargo:rustc-link-lib=static=jingler_cmd");
}
