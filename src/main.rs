use std::env;
use std::process::Command;

use gst::{cast_insertion, parse, sanity_check_final_parser, type_check};

use std::ffi::OsStr;
use std::path::Path;

use miette::Result;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: cargo run [filename]");
        return Ok(());
    }

    let mut filename = &args[1];
    let full_path = Path::new(filename).canonicalize().unwrap();
    let full_path = full_path.as_os_str().to_str().unwrap();
    let base_filename = Path::new(filename)
        .file_name()
        .and_then(OsStr::to_str)
        .unwrap();
    let dir_path = &full_path[..(full_path.len() - base_filename.len())];
    let file_stem = Path::new(filename)
        .file_stem()
        .and_then(OsStr::to_str)
        .unwrap();
    let mut new_filename: String; // If .erl to .core happens

    // If .erl file is given, then compile this file
    if let Some("erl") = Path::new(filename).extension().and_then(OsStr::to_str) {
        let status = Command::new("erlc")
            .arg("+to_core")
            .arg("-o")
            .arg(dir_path)
            .arg(filename)
            .status()
            .expect("");
        if status.code().unwrap() > 0 {
            // TODO: Sensible way to get rid of panic here?
            panic!("Could not convert erl to core")
        }
        new_filename = dir_path.to_owned();
        new_filename.push_str(file_stem);
        new_filename.push_str(".core");
        filename = &new_filename;
    }

    match std::fs::read_to_string(filename) {
        Ok(src) => {
            let (core_ast, contract) = parse(filename, &src)?;
            if !contract.warnings.is_empty() {
                println!("\nCore Erlang Contract created with the following warnings:");
                for elm in contract.warnings {
                    println!("Warning: {}", elm);
                }
            }
            let type_check_res = type_check(contract.res);
            if !type_check_res.warnings.is_empty() {
                println!("\nType checker returned the following warnings:");
                for elm in type_check_res.warnings {
                    println!("Warning: {}", elm);
                }
            } else {
                println!("\nThere were no warnings from the type checker.")
            }

            // TODO: Move away from main, cast insertion should not happen in main.rs ! But for now
            // just to test it is here
            if !type_check_res.cast_env.0.is_empty() {
                println!(
                    "There is something to change. Sample output now: {}",
                    core_ast
                );
                // 1) Sanity check parse again and check AST the same ?
                let sanity_check_new_src = format!("{}", core_ast.clone());
                match sanity_check_final_parser(&sanity_check_new_src) {
                    Ok(core_ast_control) => {
                        if core_ast == core_ast_control {
                            println!("All good");
                        } else {
                            // TODO: How to compare AST without location difference false positive?
                            // println!("Oh no, not the same {:?} {:?}", core_ast, core_ast_control);
                            println!("Cannot confirm equality of AST.")
                        }
                    }
                    err => todo!("Err: {:?}\nDEBUG:\n{:?}", err, core_ast),
                };

                // 2) Try to do the cast insertion now ?
                println!("{:?}", type_check_res.cast_env);
                let res = cast_insertion(&type_check_res.cast_env, &core_ast);
                println!("RES: {}", res);
            }

            if type_check_res.res {
                println!("\nResult: PASS\n");
            } else {
                println!("\nResult: FAIL\n");
            };
            Ok(())
        } // TODO: Pretty print
        // TODO: Sensible way to get rid of panic here?
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
