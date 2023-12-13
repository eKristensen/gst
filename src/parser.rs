use nom::{IResult, sequence::{delimited, tuple, pair}, character::complete::{multispace0, digit1, anychar}, multi::{separated_list0, many0}, combinator::{map_res, opt, value, map, peek}, branch::alt, number::complete::float, error::ParseError, Parser, bytes::complete::is_not};
use crate::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Expr};
use nom::bytes::complete::{tag, take_until};
use crate::ast::Const::List;
use crate::parser::Lit::EmptyList;
use crate::parser::Lit::Int;
use crate::parser::Lit::Float;

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#-ceol-style-comments
// Comment
fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
    // TODO: Better to use char('%') instead of tag("%") ??
    map(pair(nom::character::complete::char('%'), take_until("\n")),|(_,_)| ())(i) // TODO: Better to use end of line instead of is_not ? TODO: Type of line endings...
}

// Removes annotation
fn annotation<'a, F, O, E: ParseError<&'a str>>(inner: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
F: Parser<&'a str, O, E>,
{
    map(
        tuple((
            ws(tag("(")),
            inner,
            ws(tag("-|")),
            take_until(")"),
            ws(tag(")")),
        ))
    ,|(_,o,_,_,_)| o)
}

fn opt_annotation<'a, F, O, E: ParseError<&'a str>>(inner: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
F: Parser<&'a str, O, E> + Clone,
{
    alt((
        inner.clone(),
        annotation(inner),
    ))
}

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#wrapper-combinators-that-eat-whitespace-before-and-after-a-parser
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and 
/// trailing whitespace, returning the output of `inner`.
// TODO: Stupid comment implementation maybe something better exists?
fn ws<'a, F, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
  where
  F: Parser<&'a str, O, E>,
  &'a str: nom::InputLength + nom::InputTakeAtPosition + Clone,
  <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
  <&'a str as nom::InputTakeAtPosition>::Item: Clone
{
  map(
  tuple((
    multispace0,
    opt(comment),
    multispace0,
    inner,
    multispace0,
    opt(comment),
    multispace0,
  )),
   |(_,_,_,o,_,_,_)| o)
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

// General TODO: Maybe avoid manual OK((i, sth)) return use map instead?

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
    ws(map(tuple((
        atom,
        tag("/"), // TODO: Check whether whitespace is allowed around this tag in this context
        integer
    )),|(name,_,arity)| FunHead{name:Fname(name), arity: arity}))(i)
}

// const is a keyword in rust, const_ is used instead
fn const_(i: &str) -> IResult<&str, Const> {
    alt((
        map(lit, crate::ast::Const::Lit),
        const_nested_list,
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
    ws(map(tuple((
        atom,
        ws(tag("=")),
        const_
    )),|(atom,_,val)| Attribute{name:atom, value: val}))(i)
}

// TODO: Common pattern for nested list, avoid manual rewrite!
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

// TODO: Common pattern for nested list, avoid manual rewrite!
fn const_nested_list(i: &str) -> IResult<&str, Const> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, constant) = ws(const_)(i)?;
    let head =
        match constant {
            Const::List(inner_list) => inner_list,
            _ => vec![constant]
        };

    let (i, _) = ws(tag("|"))(i)?;
    let (i, constant) = ws(const_)(i)?;
    let tail =
        match constant {
            Const::List(inner_list) => inner_list,
            _ => vec![constant]
        };
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::ast::Const::List(cons)))
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt(( // TODO: Add var
        map(fname, crate::ast::Expr::Fname),
        map(lit, crate::ast::Expr::Lit),
        map(fun, |fun| crate::ast::Expr::Fun(Box::new(fun))),
        expr_nested_list,
        map(comma_sep_list("[", "]", exprs), crate::ast::Expr::List),
        map(comma_sep_list("{", "}", exprs), crate::ast::Expr::Tuple),
    ))(i)
}

fn exprs(i: &str) -> IResult<&str, Expr> {
    alt((
        opt_annotation(expr),
        map(comma_sep_list("<", ">", expr),crate::ast::Expr::List)
    ))(i)
}

fn fun(i: &str) -> IResult<&str, FunDef> {
    let (i, head) = fname(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, (_, exprs)) = opt_annotation(fun_inner)(i)?;

    Ok((i, FunDef{head, args: vec![], body: exprs}))
}

fn fun_inner(i: &str) -> IResult<&str, (Vec<()>, Expr)> {
    let (i, _) = ws(tag("fun"))(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, _) = ws(tag("->"))(i)?;

    let (i, exprs) = ws(exprs)(i)?;
    Ok((i, (vec![], exprs)))
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
    let (i, body) = many0(ws(fun))(i)?;

    // Require end keyword
    let (i, _) = ws(tag("end"))(i)?;


    Ok((i,Module {
        name: name,
        exports: exports,
        attributes: attributes,
        body: body
    }))
}
