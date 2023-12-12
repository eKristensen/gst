use nom::{IResult, sequence::{delimited, tuple}, character::complete::{multispace0, digit1, anychar}, multi::separated_list0, combinator::{map_res, opt, value, map}, branch::alt, number::complete::float, error::ParseError, Parser, Compare};
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
        const_list,
        const_tuple
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

// TODO: There should be a smarter way
fn const_tuple(i: &str) -> IResult<&str, Const> {
    let (i, const_list) = delimited(
        tag("{"),
        separated_list0(tag(","),const_),
        tag("}")
    )(i)?;
    Ok((i, crate::ast::Const::Tuple(const_list)))
}

fn attribute(i: &str) -> IResult<&str, Attribute> {
    let (i,(atom,_,val)) = ws(tuple((
        atom,
        tag("="),
        const_
    )))(i)?;
    Ok((i,Attribute{name:atom, value: val}))
}

// TODO: General from_list (pass fname function as a parameter, how to get it to work?)
fn fname_list(i: &str) -> IResult<&str, Vec<FunHead>> {
    delimited(
        tag("["),
        separated_list0(tag(","),fname),
        tag("]")
    )(i)
}

// TODO: General from_list (pass fname function as a parameter, how to get it to work?)
fn attribute_list(i: &str) -> IResult<&str, Vec<Attribute>> {
    delimited(
        tag("["),
        separated_list0(tag(","),attribute),
        tag("]")
    )(i)
}

// TODO: General from_list (pass fname function as a parameter, how to get it to work?)
fn const_list(i: &str) -> IResult<&str, Const> {
    let (i, const_list) = delimited(
        tag("["),
        separated_list0(tag(","),const_),
        tag("]")
    )(i)?;
    Ok((i, List(const_list)))
}

fn expr_list_head(i: &str) -> IResult<&str, Expr> {
    let (i, _) = tag("[")(i)?;
    let (i, expr) = exprs(i)?;
    let head = vec![expr];
    let (i, tail) = expr_list_tail(i)?;
    // TODO: Flatten nested lists. Does not work right now
    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::ast::Expr::List(cons)))
}

fn expr_list_tail(i: &str) -> IResult<&str, Vec<Expr>> {
    alt((
        value(vec![], tag("]")),
        expr_list_tail_bar,
        expr_list_tail_comma
    ))(i)
}

fn expr_list_tail_bar(i: &str) -> IResult<&str, Vec<Expr>> {
    let (i, _) = tag("|")(i)?;
    let (i, exprs) = exprs(i)?;
    let (i, _) = tag("]")(i)?;
    Ok((i, vec![exprs]))
}

fn expr_list_tail_comma(i: &str) -> IResult<&str, Vec<Expr>> {
    let (i, _) = tag(",")(i)?;
    let (i, expr) = exprs(i)?;
    let head = vec![expr];
    let (i, tail) = expr_list_tail(i)?;
    let cons = [&head[..], &tail[..]].concat();
    Ok((i, cons))

}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt(( // TODO: Add var
        map(fname, crate::ast::Expr::Fname),
        map(lit, crate::ast::Expr::Lit),
        expr_fun,
        expr_list_head,
        expr_tuple,
    ))(i)
}

fn expr_fun(i: &str) -> IResult<&str, Expr> {
    let (i, fun) = fun(i)?;
    Ok((i, crate::ast::Expr::Fun(Box::new(fun))))
}

fn expr_tuple(i: &str) -> IResult<&str, Expr> {
    alt((
        empty_expr_tuple,
        some_expr_tuple
    ))(i)
}

fn empty_expr_tuple(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("{"))(i)?;
    let (i, _) = ws(tag("}"))(i)?;
    Ok((i, crate::ast::Expr::Tuple(vec![])))
}

// TODO: General from_list (pass fname function as a parameter, how to get it to work?)
fn some_expr_tuple(i: &str) -> IResult<&str, Expr> {
    let (i, expr_tuple) = delimited(
        tag("{"),
        separated_list0(tag(","),exprs),
        tag("}")
    )(i)?;
    Ok((i, crate::ast::Expr::List(expr_tuple)))
}


// TODO: General from_list (pass fname function as a parameter, how to get it to work?)
fn expr_value_list(i: &str) -> IResult<&str, Expr> {
    let (i, expr_value_list) = delimited(
        tag("<"),
        separated_list0(tag(","),expr),
        tag(">")
    )(i)?;
    Ok((i, crate::ast::Expr::List(expr_value_list)))
}

fn exprs(i: &str) -> IResult<&str, Expr> {
    alt((
        expr,
        expr_value_list
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
    Ok((i, FunDef{head: head, args: vec![], body: exprs}))
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
    let (i, exports) = ws(fname_list)(i)?;

    // Require attributes keyword
    let (i, _) = ws(tag("attributes"))(i)?;

    // Get attributes
    let (i, attributes) = ws(attribute_list)(i)?;

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
