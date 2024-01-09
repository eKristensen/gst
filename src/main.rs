mod cerl_parser;
mod st_parser;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return println!("Usage: cargo run [filename]");
    }

    let filename = &args[1];

    match std::fs::read_to_string(filename) {
        Ok(src) => println!(
            "Ran parser with debug AST output: {:?}",
            cerl_parser::top::module(&src)
        ), // TODO: Pretty print
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
