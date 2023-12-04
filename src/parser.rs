
use nom::{IResult, sequence::{delimited}, character::complete::{multispace0}};
use crate::ast::Module;
use nom::bytes::complete::{tag, take_until};


// TODO: Accept atoms with escaped chars
// https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
fn atom(i: &str) -> IResult<&str, &str> {
    delimited(
        tag("'"),
        take_until("'"),
        tag("'")
    )(i)
}

// TODO: Ignores content for now, should properly parse it
fn from_list(i: &str) -> IResult<&str, &str> {
    delimited(
        tag("["),
        take_until("]"),
        tag("]")
    )(i)
}

// Top level module definition
pub fn module(i: &str) -> IResult<&str, Module> {
    // TODO: Check that "tag" requires "module" and does not work with partial data such as "mod"
    // Scan module
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("module")(i)?;
    let (i, _) = multispace0(i)?;

    // Get module name.
    let (i, name) = atom(i)?;
    let (i, _) = multispace0(i)?;

    // Get exports
    let (i, _) = from_list(i)?;
    let (i, _) = multispace0(i)?;

    // Require attributes keyword
    let (i, _) = tag("attributes")(i)?;
    let (i, _) = multispace0(i)?;

    // Get attributes
    let (i, _) = from_list(i)?;
    let (i, _) = multispace0(i)?;

    // Require end keyword
    let (i, _) = tag("end")(i)?;
    let (i, _) = multispace0(i)?;


    Ok((i,Module {
        name: name.to_string(),
        exports: vec![],
        attributes: vec![],
        body: vec![]
    }))
}
