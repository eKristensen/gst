
use std::iter;

use nom::{IResult, sequence::{delimited, tuple, Tuple}, character::complete::{multispace0, one_of, digit1, anychar}, multi::{many1, separated_list0}, combinator::{recognize, map_res, opt, value}, branch::alt, number::complete::float};
use crate::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Var, Expr};
use nom::bytes::complete::{tag, take_until};
use crate::ast::Const::List;
use crate::parser::Lit::EmptyList;
use crate::parser::Lit::Int;
use crate::parser::Lit::Float;


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
    let (i, integer) = map_res(digit1, str::parse)(i)?;
    Ok((i, Integer(integer)))
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
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
    let (i,(name,_,arity)) = tuple((
        atom,
        tag("/"),
        integer
    ))(i)?;
    Ok((i,FunHead{name:Fname(name), arity: arity}))
}

// const is a keyword in rust, const_ is used instead
fn const_(i: &str) -> IResult<&str, Const> {
    alt((
        const_lit,
        const_list,
        const_tuple
    ))(i)
}

fn const_lit(i: &str) -> IResult<&str, Const> {
    let (i, val) = lit(i)?;
    Ok((i, crate::ast::Const::Lit(val)))
}

fn lit(i: &str) -> IResult<&str, Lit> {
    alt((
        lit_integer,
        lit_float,
        lit_atom,
        lit_char,
        lit_string,
        empty_list
    ))(i)
}

// TODO: Stupid type conversion function....
fn lit_integer(i: &str) -> IResult<&str, Lit> {
    let (i, integer) = integer(i)?;
    Ok((i, Int(integer)))
}

// TODO: Stupid type conversion function....
fn lit_float(i: &str) -> IResult<&str, Lit> {
    let (i, float) = float(i)?;
    Ok((i, Float(float)))
}

// TODO: Stupid type conversion function....
fn lit_atom(i: &str) -> IResult<&str, Lit> {
    let (i, atom) = atom(i)?;
    Ok((i, crate::ast::Lit::Atom(atom)))
}

// TODO: Stupid type conversion function....
fn lit_char(i: &str) -> IResult<&str, Lit> {
    let (i, char) = char_(i)?;
    Ok((i, crate::ast::Lit::Char(char)))
}

// TODO: Stupid type conversion function....
fn lit_string(i: &str) -> IResult<&str, Lit> {
    let (i, string) = string(i)?;
    Ok((i, crate::ast::Lit::String(string)))
}

fn char_(i: &str) -> IResult<&str, char> {
    let (i, _) = tag("$")(i)?; // Char starts with $ in erlang
    anychar(i) // TODO: Check anychar is acceptable according to the spec
}

fn empty_list(i: &str) -> IResult<&str, Lit> {
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
    let (i, _) = tag("[")(i)?;
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
    let (i, _) = tag("]")(i)?;
    Ok((i, EmptyList))
}

fn const_tuple(i: &str) -> IResult<&str, Const> {
    let (i, const_list) = delimited(
        tag("{"),
        separated_list0(tag(","),const_),
        tag("}")
    )(i)?;
    Ok((i, crate::ast::Const::Tuple(const_list)))
}

fn attribute(i: &str) -> IResult<&str, Attribute> {
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
    let (i,(atom,_,val)) = tuple((
        atom,
        tag("="),
        const_
    ))(i)?;
    let (i, _) = multispace0(i)?; // TODO Less ugly/generic way to remove whitespace in between items in a list??
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

fn expr_fname(i: &str) -> IResult<&str, Expr> {
    let (i, fname) = fname(i)?;
    Ok((i, crate::ast::Expr::Fname(fname)))
}

fn expr_lit(i: &str) -> IResult<&str, Expr> {
    let (i, lit) = lit(i)?;
    Ok((i, crate::ast::Expr::Lit(lit)))
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt(( // TODO: Add var
        expr_fname,
        expr_lit,
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
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("{")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("}")(i)?;
    let (i, _) = multispace0(i)?;
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
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("fun")(i)?;
    let (i, _) = multispace0(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = multispace0(i)?;

    let (i, exprs) = exprs(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, FunDef{head: head, args: vec![], body: exprs}))
}

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
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("module")(i)?;
    let (i, _) = multispace0(i)?;

    // Get module name.
    let (i, name) = atom(i)?;
    let (i, _) = multispace0(i)?;

    // Get exports
    let (i, exports) = fname_list(i)?;
    let (i, _) = multispace0(i)?;

    // Require attributes keyword
    let (i, _) = tag("attributes")(i)?;
    let (i, _) = multispace0(i)?;

    // Get attributes
    let (i, attributes) = attribute_list(i)?;
    let (i, _) = multispace0(i)?;

    // Module Body - Function definitions
    let (i, body) = opt_fun(i)?;
    let (i, _) = multispace0(i)?;

    // Require end keyword
    let (i, _) = tag("end")(i)?;
    let (i, _) = multispace0(i)?;


    Ok((i,Module {
        name: name,
        exports: exports,
        attributes: attributes,
        body: body
    }))
}
