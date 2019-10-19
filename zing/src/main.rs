
#[macro_use] extern crate lalrpop_util;

mod ast;
#[macro_use] mod builtin;
mod bytecodes;
mod code_generator;
mod compiler;
mod names;
mod pretty_print;
mod type_inference;

use std::env;
use std::fs;

fn main() {
	for arg in env::args().skip(1) {
		match fs::read_to_string(&arg) {
			Ok(s) => match compiler::Compiler::new(&arg, &s).compile() {
				Ok(program) => println!("{}", program),
				Err(errors) => for message in errors {
					println!("{}", message);
				}
			},
			Err(e) => {
				println!("Error reading '{}': {}", arg, e);
			}
		}
	}
}
