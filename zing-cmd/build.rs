extern crate nasm_rs;

fn main() {
	#[cfg(not(target_env = "msvc"))]
	nasm_rs::compile_library("libclinklang_cmd.a", &["../runtime/cmd.asm"]);

	#[cfg(target_env = "msvc")]
	nasm_rs::compile_library("clinklang_cmd.lib", &["../runtime/cmd.asm"]);

	println!("cargo:rustc-link-lib=static=clinklang_cmd");
}
