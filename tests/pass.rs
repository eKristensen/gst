#[cfg(test)]

// Integration tests / Black box tests

use gst;

extern crate test_generator;
use test_generator::test_resources; // TODO: Small lib, maybe use something else?

use std::process::Command;

// TODO Verify .core file is correctly translated from .erl
// #[test_resources("tests/pass/*.erl")]
// fn erl_core_consistentcy

// TODO Verify that erlc accepts all *.core files
#[test_resources("tests/pass/*.core")]
fn erlc_acceptance(resource: &str) {
    let status = Command::new("erlc")
        .arg(resource)
        .status()
        .expect("");
    assert_eq!(0, status.code().unwrap());
}

// Verify that all file in pass folder compiles without any error
#[test_resources("tests/pass/*.core")]
fn can_parse(resource: &str) {
    match std::fs::read_to_string(resource) {
        Ok(src) => assert!(gst::parse(&src).is_ok()),
        Err(err) => panic!("Could not read file {}", err),
    }
}
