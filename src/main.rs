mod analysis;
mod cerl_parser;
mod st_parser;

use std::env;

use crate::analysis::{analyze_st::analyze_module, compute_init_env::init_module_env};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return println!("Usage: cargo run [filename]");
    }

    let filename = &args[1];

    match std::fs::read_to_string(filename) {
        Ok(src) => {
            println!(
                "Ran parser with debug AST output: {:?}",
                cerl_parser::top::module(&src)
            );
            match cerl_parser::top::module(&src) {
                Ok((_, module)) => {
                    let env = init_module_env(module);
                    println!("Init analysis environment {:?}", env);
                    analyze_module(env);
                }
                Err(_) => println!("Nom could not parse source"),
            }
        } // TODO: Pretty print
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
