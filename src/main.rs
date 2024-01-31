mod analysis;
mod cerl_parser;
mod st_parser;

use std::env;

use crate::analysis::{analyze_var::analyze_module, compute_init_env::init_module_env};

use nom::Err;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return println!("Usage: cargo run [filename]");
    }

    let filename = &args[1];

    match std::fs::read_to_string(filename) {
        Ok(src) => {
            // println!(
            //     "Ran parser with debug AST output: {:?}\n",
            //     cerl_parser::top::module(&src)
            // );
            match cerl_parser::top::module(&src) {
                // TODO: Add ".finish()" here and in tests or even better in common module.
                Ok((_, module)) => {
                    let env = init_module_env(module);
                    //println!("Init analysis environment {:?}\n", env);
                    analyze_module(&env);
                }
                Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                    println!("Nom could not parse source\n\n{}", e);
                }
                _ => panic!("Unknown error"),
            }
        } // TODO: Pretty print
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
