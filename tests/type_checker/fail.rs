#[cfg(test)]
// Integration tests / Black box tests
use gst;

extern crate test_generator;
use test_generator::test_resources; // TODO: Small lib, maybe use something else?

use std::process::Command;

// TODO Verify .core file is correctly translated from .erl
#[test_resources("tests/type_checker/fail/*.erl")]
fn type_checker_fail_erl_core_consistency(resource: &str) {
    let mut core_file: String = resource[0..resource.len() - 4].to_string();
    core_file.push_str(".core");

    match std::fs::read_to_string(core_file.clone()) {
        Ok(original) => {
            let status = Command::new("erlc")
                .arg("+to_core")
                .arg("-o")
                .arg("tests/type_checker/fail")
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
#[test_resources("tests/type_checker/fail/*.core")]
fn type_checker_fail_erlc_acceptance(resource: &str) {
    let status = Command::new("erlc")
        .arg(resource)
        .status()
        .expect("failed to execute process");
    assert_eq!(0, status.code().unwrap());
}

// Verify that all file in fail folder compiles without any error
#[test_resources("tests/type_checker/fail/*.core")]
fn type_checker_fail_expect_rejection(resource: &str) {
    let src = std::fs::read_to_string(resource).unwrap();
    let module = gst::parse(&src).unwrap();
    // Special case for some files
    match resource {
        "tests/type_checker/fail/client.core" => assert_eq!(module.warnings.len(), 0),
        _ => assert_eq!(module.warnings.is_empty(), false),
    }
    let typed = gst::type_check(module.res);
    // Special case for some files
    match resource {
        "tests/type_checker/fail/client.core" => assert_eq!(typed.warnings.len(), 1),
        _ => assert_eq!(typed.warnings.is_empty(), false),
    }
    assert_eq!(typed.res, false);
}
