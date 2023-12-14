use std::ops::RangeFrom;

use nom::{IResult, sequence::{delimited, tuple, pair, preceded}, character::{complete::{multispace0, digit1, anychar}, is_digit}, multi::{separated_list0, many0, fold_many0}, combinator::{map_res, opt, value, map, peek}, branch::alt, number::complete::float, error::{ParseError, ErrorKind}, Parser, bytes::complete::is_not, InputTakeAtPosition, AsChar, InputTake, InputIter, InputLength, Slice};
use crate::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Expr};
use nom::bytes::complete::{tag, take_until};
use crate::ast::Const::List;
use crate::parser::Lit::EmptyList;
use crate::parser::Lit::Int;
use crate::parser::Lit::Float;
use nom::character::complete::char;
use nom::Err;

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

#[inline]
fn is_sign(chr: u8) -> bool {
  chr <= 0x2B || chr == 0x2D
}

#[inline]
fn is_lowercase(chr: u8) -> bool {
  (chr >= 0x61 && chr <= 0x7A) || (chr >= 0xC0 && chr <= 0xD6) || (chr >= 0xD8 && chr <= 0xDE)
}

#[inline]
fn is_uppercase(chr: u8) -> bool {
  (chr >= 0x41 && chr <= 0x5A) || (chr >= 0xDF && chr <= 0xF6) || (chr >= 0xF8 && chr <= 0xFF)
}

#[inline]
fn is_inputchar(chr: u8) -> bool {
  chr != 0x0D && chr != 0x0A
}

#[inline]
fn is_control(chr: u8) -> bool {
  chr >= 0x00 && chr <= 0x1F
}

#[inline]
fn is_space(chr: u8) -> bool {
  chr == 0x20
}

#[inline]
fn is_namechar(chr: u8) -> bool {
  is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == 0x40 || chr == 0x5F
}

// Built into nom: is_oct_digit(chr)

fn is_ctlchar(chr: u8) -> bool {
  chr >= 0x40 && chr <= 0x5F
}

fn is_escapechar(chr: u8) -> bool {
   chr == 0x62 || chr == 0x64 || chr == 0x65 || chr == 0x66 || chr == 0x6E || chr == 0x72 || chr == 0x73 || chr == 0x74 || chr == 0x76 || chr == 0x22 || chr == 0x27 || chr == 0x5C
}

// Based on: https://github.com/rust-bakery/nom/blob/main/examples/string.rs
/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
  E: ParseError<&'a str>,
{
  preceded(
    char('\\'),
    alt((
      char('b'),
      char('d'),
      char('e'),
      char('f'),
      value('\n', char('n')),
      value('\r', char('r')),
      char('s'),
      value('\t', char('t')),
      char('v'),
      char('\''),
      char('\\'),
    )),
  )
  .parse(input)
}

// General TODO: Maybe avoid manual OK((i, sth)) return use map instead?

fn parse_atom_input_chr<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: InputTakeAtPosition,
  <T as InputTakeAtPosition>::Item: AsChar,
{
  input.split_at_position1_complete(|item| 
    {
        let item_chr = item.as_char() as u8; // TODO extend trait instead of this
          !(is_inputchar(item_chr)) // With trait it could be item.is_inputchar()
        ||  is_control(item_chr)
        ||  item_chr == 0x5C // Hex codes are not easy to read...
        ||  item_chr == 0x27
    }, nom::error::ErrorKind::Fix // TODO: Actual error message
    )
}

fn parse_atom_fragments(i: &str) -> IResult<&str, String> {
    alt((
        map(parse_atom_input_chr,|o: &str| o.to_string()),
        map(parse_escaped_char, |o| o.to_string())
    ))(i)
}

// TODO: Accept atoms with escaped chars
// Inspired by :
// - https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
// - https://github.com/rust-bakery/nom/blob/main/examples/string.rs
fn atom(i: &str) -> IResult<&str, Atom> {
  // fold is the equivalent of iterator::fold. It runs a parser in a loop,
  // and for each output value, calls a folding function on each output value.
  let build_string = fold_many0(
    // Our parser function– parses a single string fragment
    parse_atom_fragments,
    // Our init value, an empty string
    String::new,
    // Our folding function. For each fragment, append the fragment to the
    // string.
    |mut string, fragment| {
        string.push_str(&fragment);
        string
    },
  );

  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold), be sure that the
  // loop won't accidentally match your closing delimiter!
  map(delimited(char('\''), build_string, char('\'')),|o| Atom(o.to_string())).parse(i)
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
  let (i, _) = char('$')(i)?;
  alt((
    char_name,
    parse_escaped_char
  ))(i)
}

fn char_name<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
where
  T: InputIter + InputLength + Slice<RangeFrom<usize>>,
  <T as InputIter>::Item: AsChar,
{
  let mut it = input.iter_indices();
  let (input, candidate) = match it.next() {
    None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
    Some((_, c)) => match it.next() {
      None => Ok((input.slice(input.input_len()..), c.as_char())),
      Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
    },
  }?;
  let item_chr = candidate.as_char() as u8; // TODO extend trait instead of this
  if !(is_inputchar(item_chr)) // With trait it could be item.is_inputchar()
  ||  is_control(item_chr)
  ||  item_chr == 0x20 // Hex codes are not easy to read...
  ||  item_chr == 0x5C {
    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
  }
  Ok((input, candidate))
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


    Ok((i,Module {name, exports, attributes, body}))
}
