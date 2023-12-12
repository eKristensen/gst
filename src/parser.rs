use nom::{IResult, sequence::{delimited, tuple}, character::complete::{multispace0, digit1, anychar}, multi::separated_list0, combinator::{map_res, opt, value, map}, branch::alt, number::complete::float, error::ParseError, Parser};
use crate::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Expr};
use nom::bytes::complete::{tag, take_until};
use crate::ast::Const::List;
use crate::parser::Lit::EmptyList;
use crate::parser::Lit::Int;
use crate::parser::Lit::Float;

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#wrapper-combinators-that-eat-whitespace-before-and-after-a-parser
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and 
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F, I, O, E: ParseError<I>>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
  where
  F: Parser<I, O, E>,
  I: nom::InputLength + nom::InputTakeAtPosition,
  <I as nom::InputTakeAtPosition>::Item: nom::AsChar,
  <I as nom::InputTakeAtPosition>::Item: Clone
{
  delimited(
    multispace0,
    inner,
    multispace0
  )
}

// General helper for comma-separated lists
// TODO: Maybe too general? Some lists may require a least one element... e.g. is expr "<>" allowed? is "{}" allowed?
fn comma_sep_list<'a, O, E: ParseError<&'a str>, F>(
    start: &'a str,
    end: &'a str,
    elements: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    F: Parser<&'a str, O, E>,
{
delimited(tag(start), separated_list0(tag(","), elements), tag(end))
}

// TODO: Accept atoms with escaped chars
// https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
fn atom(i: &str) -> IResult<&str, Atom> {
    let (i, atom) = delimited(
        tag("'"),
        take_until("'"), // TODO: Check for valid input here. Anything is accepted right now
        tag("'")
    )(i)?;
    Ok((i, Atom(atom.to_string()))) // TODO: Something better than to_string() ???
}

fn integer(i: &str) -> IResult<&str, Integer> {
    map(map_res(digit1, str::parse),Integer)(i)
}

fn string(i: &str) -> IResult<&str, String> {
    let (i, string) = delimited(
        tag("\""),
        take_until("\""), // TODO: Check for valid input here. Anything is accepted right now
        tag("\"")
    )(i)?;
    Ok((i, string.to_string())) // TODO: Something better than to_string() ???
}

fn fname(i: &str) -> IResult<&str, FunHead> {
    let (i,(name,_,arity)) = ws(tuple((
        atom,
        tag("/"), // TODO: Check whether whitespace is allowed around this tag in this context
        integer
    )))(i)?;
    Ok((i,FunHead{name:Fname(name), arity: arity}))
}

// const is a keyword in rust, const_ is used instead
fn const_(i: &str) -> IResult<&str, Const> {
    alt((
        map(lit, crate::ast::Const::Lit),
        map(comma_sep_list("[", "]", const_),List),
        map(comma_sep_list("{", "}", const_), crate::ast::Const::Tuple),
    ))(i)
}

fn lit(i: &str) -> IResult<&str, Lit> {
    alt((
        map(integer, Int),
        map(float, Float),
        map(atom, crate::ast::Lit::Atom),
        map(char_, crate::ast::Lit::Char),
        map(string, crate::ast::Lit::String),
        empty_list
    ))(i)
}

fn char_(i: &str) -> IResult<&str, char> {
    let (i, _) = tag("$")(i)?; // Char starts with $ in erlang
    anychar(i) // TODO: Check anychar is acceptable according to the spec
}

fn empty_list(i: &str) -> IResult<&str, Lit> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, _) = ws(tag("]"))(i)?;
    Ok((i, EmptyList))
}

fn attribute(i: &str) -> IResult<&str, Attribute> {
    let (i,(atom,_,val)) = ws(tuple((
        atom,
        tag("="),
        const_
    )))(i)?;
    Ok((i,Attribute{name:atom, value: val}))
}

fn expr_nested_list(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, expr) = ws(exprs)(i)?;
    let head =
        match expr {
            Expr::List(inner_list) => inner_list,
            _ => vec![expr]
        };

    let (i, _) = ws(tag("|"))(i)?;
    let (i, expr) = ws(exprs)(i)?;
    let tail =
        match expr {
            Expr::List(inner_list) => inner_list,
            _ => vec![expr]
        };
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::ast::Expr::List(cons)))
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt(( // TODO: Add var
        map(fname, crate::ast::Expr::Fname),
        map(lit, crate::ast::Expr::Lit),
        expr_fun,
        expr_nested_list,
        map(comma_sep_list("[", "]", exprs), crate::ast::Expr::List),
        map(comma_sep_list("{", "}", exprs), crate::ast::Expr::Tuple),
    ))(i)
}

fn expr_fun(i: &str) -> IResult<&str, Expr> {
    let (i, fun) = fun(i)?;
    Ok((i, crate::ast::Expr::Fun(Box::new(fun))))
}

fn exprs(i: &str) -> IResult<&str, Expr> {
    alt((
        expr,
        map(comma_sep_list("<", ">", expr),crate::ast::Expr::List)
    ))(i)
}

fn fun(i: &str) -> IResult<&str, FunDef> {
    let (i, head) = fname(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, _) = ws(tag("fun"))(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, _) = ws(tag("->"))(i)?;

    let (i, exprs) = ws(exprs)(i)?;
    Ok((i, FunDef{head, args: vec![], body: exprs}))
}

// TODO: There should be a better way to wrap an option type...
fn opt_fun(i: &str) -> IResult<&str, Vec<FunDef>> {
    let (i, maybe_body) = opt(fun)(i)?;
    match maybe_body {
        Some(body) => Ok((i,vec![body])),
        None => Ok((i,vec![])),
    }
}

// Top level module definition
pub fn module(i: &str) -> IResult<&str, Module> {
    // TODO: Check that "tag" requires "module" and does not work with partial data such as "mod"
    // Scan module
    let (i, _) = ws(tag("module"))(i)?;

    // Get module name.
    let (i, name) = ws(atom)(i)?;

    // Get exports
    let (i, exports) = ws(comma_sep_list("[", "]", fname))(i)?;

    // Require attributes keyword
    let (i, _) = ws(tag("attributes"))(i)?;

    // Get attributes
    let (i, attributes) = ws(comma_sep_list("[", "]", attribute))(i)?;

    // Module Body - Function definitions
    let (i, body) = ws(opt_fun)(i)?;

    // Require end keyword
    let (i, _) = ws(tag("end"))(i)?;


    Ok((i,Module {
        name: name,
        exports: exports,
        attributes: attributes,
        body: body
    }))
}
