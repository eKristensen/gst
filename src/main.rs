mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;

use std::env;
use std::process::Command;

use gst::parse;

use std::ffi::OsStr;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return println!("Usage: cargo run [filename]");
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
            panic!("Could not convert erl to core")
        }
        new_filename = dir_path.to_owned();
        new_filename.push_str(file_stem);
        new_filename.push_str(".core");
        filename = &new_filename;
    }

    match std::fs::read_to_string(filename) {
        Ok(src) => {
            println!("Contract core erlang debug text: {:?}", parse(&src))
        } // TODO: Pretty print
        Err(err) => panic!("Could not read file {} because {}", filename, err),
    }
}
