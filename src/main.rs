mod parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return println!("Usage: cargo run [filename]")
    }

    let filename = &args[1];

    match std::fs::read_to_string(filename) {
        Ok(src) => println!("Parsed with debug AST output: {:?}", parser::parser::module(&src)), // TODO: Pretty print
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
