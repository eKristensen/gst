#[cfg(test)]
// Integration tests / Black box tests
use gst;

extern crate test_generator;
use test_generator::test_resources; // TODO: Small lib, maybe use something else?

use std::process::Command;

// TODO Verify .core file is correctly translated from .erl
#[test_resources("tests/analysis/fail/*.erl")]
fn analysis_fail_erl_core_consistency(resource: &str) {
    let mut core_file: String = resource[0..resource.len() - 4].to_string();
    core_file.push_str(".core");

    match std::fs::read_to_string(core_file.clone()) {
        Ok(original) => {
            let status = Command::new("erlc")
                .arg("+to_core")
                .arg("-o")
                .arg("tests/analysis/fail")
                .arg(resource)
                .status()
                .expect("");
            assert_eq!(0, status.code().unwrap());
            match std::fs::read_to_string(core_file) {
                // Read again
                Ok(freshly_compiled) => assert_eq!(original, freshly_compiled),
                Err(err) => panic!("Could not read file {}", err),
            }
        }
        Err(err) => panic!("Could not read file {}", err),
    }
}

// TODO Verify that erlc accepts all *.core files
#[test_resources("tests/analysis/fail/*.core")]
fn analysis_fail_erlc_acceptance(resource: &str) {
    let status = Command::new("erlc")
        .arg(resource)
        .status()
        .expect("failed to execute process");
    assert_eq!(0, status.code().unwrap());
}

// Verify that all file in fail folder compiles without any error
#[test_resources("tests/analysis/fail/*.core")]
fn analysis_fail_expect_rejection(resource: &str) {
    let src = std::fs::read_to_string(resource).unwrap();
    let (_, ast) = gst::parse(&src).unwrap();
    assert_eq!(false, gst::analyze(ast))
}
