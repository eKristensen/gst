#[cfg(test)]
// Integration tests / Black box tests
extern crate test_generator;
use test_generator::test_resources; // TODO: Small lib, maybe use something else?

use std::process::Command;

// TODO Verify .core file is correctly translated from .erl
#[test_resources("tests/type_checker/pass/*.erl")]
fn type_checker_pass_erl_core_consistency(resource: &str) {
    let mut core_file: String = resource[0..resource.len() - 4].to_string();
    core_file.push_str(".core");

    match std::fs::read_to_string(core_file.clone()) {
        Ok(original) => {
            let status = Command::new("erlc")
                .arg("+to_core")
                .arg("-o")
                .arg("tests/type_checker/pass")
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
#[test_resources("tests/type_checker/pass/*.core")]
fn type_checker_pass_erlc_acceptance(resource: &str) {
    let status = Command::new("erlc")
        .arg(resource)
        .status()
        .expect("failed to execute process");
    assert_eq!(0, status.code().unwrap());
}

// Verify that all file in pass folder are accepted
#[test_resources("tests/type_checker/pass/*.core")]
fn type_checker_pass_acceptance(resource: &str) {
    let src = std::fs::read_to_string(resource).unwrap();
    let module = gst::parse(resource, &src).unwrap();
    assert!(module.1.warnings.is_empty());
    let typed = gst::type_check(module.1.res);
    assert!(typed.warnings.is_empty());
    assert!(typed.res);
}
