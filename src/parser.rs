
use nom::{IResult, sequence::{delimited, tuple}, character::complete::{multispace0, one_of, digit1}, multi::{many1, separated_list0}, combinator::{recognize, map_res, opt}};
use crate::ast::{Module, FunHead, Fname};
use nom::bytes::complete::{tag, take_until};


// TODO: Accept atoms with escaped chars
// https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
fn atom(i: &str) -> IResult<&str, &str> {
    delimited(
        tag("'"),
        take_until("'"), // TODO: Check for valid input here. Anything is accepted right now
        tag("'")
    )(i)
}

fn fname(i: &str) -> IResult<&str, FunHead> {
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
    let (i,(name,_,arity)) = tuple((
        atom, // TODO: Better char recognize than take_until
        tag("/"),
        digit1 // TODO: Parse: https://docs.rs/nom/latest/nom/character/complete/fn.digit1.html#parsing-an-integer
    ))(i)?;
    let arity: u8 = arity.parse().unwrap(); // TODO: parse int directly with that map function in nom - instead of this
    Ok((i,FunHead{name:Fname(name.to_string()), arity: arity}))
}

// TODO: Ignores content for now, should properly parse it
fn from_list(i: &str) -> IResult<&str, Vec<FunHead>> {
    delimited(
        tag("["),
        separated_list0(tag(","),fname),
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
    let (i, exports) = from_list(i)?;
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
        exports: exports,
        attributes: vec![],
        body: vec![]
    }))
}
