#[cfg(test)]
// Integration tests / Black box tests
use gst;

extern crate test_generator;
use test_generator::test_resources; // TODO: Small lib, maybe use something else?

use std::process::Command;

// TODO Verify that erlc accepts all *.core files
#[test_resources("tests/parser/fail/*.core")]
fn erlc_rejection(resource: &str) {
    let status = Command::new("erlc")
        .arg(resource)
        .status()
        .expect("failed to execute process");
    assert_ne!(0, status.code().unwrap());
}

// Verify that all file in fail folder compiles without any error
#[test_resources("tests/parser/fail/*.core")]
fn expect_parse_error(resource: &str) {
    match std::fs::read_to_string(resource) {
        Ok(src) => assert!(gst::parse(&src).is_err()),
        Err(err) => panic!("Could not read file {}", err),
    }
}
