
use std::env;

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub zing); // Synthesized by LALRPOP

mod ast;

fn main() {
	for arg in env::args().skip(1) {
		match zing::ProgramParser::new().parse(&arg) {
			Ok(program) => println!("{:?}", program),
			Err(error) => println!("{:?}", error),
		}
	}
}
