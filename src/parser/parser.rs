use std::ops::RangeFrom;

use nom::{IResult, sequence::{delimited, tuple, pair, preceded}, character::{complete::{multispace0, digit1}, is_digit}, multi::{separated_list0, many0, fold_many0}, combinator::{map_res, opt, value, map}, branch::alt, number::complete::float, error::{ParseError, ErrorKind}, Parser, InputTakeAtPosition, AsChar, InputIter, InputLength, Slice};
use crate::parser::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Expr, Var, Clause, Pat};
use nom::bytes::complete::{tag, take_until};
use crate::parser::ast::Const::List;
use crate::parser::parser::Lit::EmptyList;
use crate::parser::parser::Lit::Int;
use crate::parser::parser::Lit::Float;
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

// TODO Can it this be integrated into atom somehow?
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

fn parse_string_input_chr<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
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
        ||  item_chr == 0x22
    }, nom::error::ErrorKind::Fix // TODO: Actual error message
    )
}

// TODO Can it this be integrated into string somehow?
fn parse_string_fragments(i: &str) -> IResult<&str, String> {
    alt((
        map(parse_string_input_chr,|o: &str| o.to_string()),
        map(parse_escaped_char, |o| o.to_string())
    ))(i)
}

// TODO: Deduplicate, a lot like fn atom
fn string(i: &str) -> IResult<&str, String> {
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
      // Our parser function– parses a single string fragment
      parse_string_fragments,
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
    map(delimited(char('"'), build_string, char('"')),|o| o.to_string()).parse(i)
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
        map(lit, crate::parser::ast::Const::Lit),
        const_nested_list,
        map(comma_sep_list("[", "]", const_),List),
        map(comma_sep_list("{", "}", const_), crate::parser::ast::Const::Tuple),
    ))(i)
}

fn lit(i: &str) -> IResult<&str, Lit> {
    alt((
        map(integer, Int),
        map(float, Float),
        map(atom, crate::parser::ast::Lit::Atom),
        map(char_, crate::parser::ast::Lit::Char),
        map(string, crate::parser::ast::Lit::String),
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

// TODO: There must exist a easier way to do this
fn uppercase_char<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
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
  if !(is_uppercase(item_chr)) // With trait it could be item.is_inputchar()
 {
    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
  }
  Ok((input, candidate))
}

// TODO: There must exist a easier way to do this
fn namechar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
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
  if !(is_namechar(item_chr)) // With trait it could be item.is_inputchar()
 {
    return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
  }
  Ok((input, candidate))
}

fn var(i: &str) -> IResult<&str, Var> {
    let (i, var_name_head) = alt((
        map(uppercase_char, |o| o.to_string()),
        map(char('_'),|o| o.to_string()), // TODO: Odd but elc accepts "_" as a valid variable name despite it explicitly being invalid in the core erlang specification
    ))(i)?;
    let (i, var_name_tail) = fold_many0(
        // Our parser function– parses a single string fragment
        namechar,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            string.push(fragment);
            string
        },
      )(i)?;
    // TODO: A bit much string manipulation, maybe inefficient...
    let mut final_var_name = var_name_head;
    final_var_name.push_str(&var_name_tail);
    Ok((i,Var(final_var_name)))
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
    Ok((i, crate::parser::ast::Expr::List(cons)))
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
    Ok((i, crate::parser::ast::Const::List(cons)))
}

// TODO: Common pattern for nested list, avoid manual rewrite!
fn pat_nested_list(i: &str) -> IResult<&str, Pat> {
  let (i, _) = ws(tag("["))(i)?;
  let (i, pattern) = ws(pat)(i)?;
  let head =
      match pattern {
          Pat::List(inner_list) => inner_list,
          _ => vec![pattern]
      };

  let (i, _) = ws(tag("|"))(i)?;
  let (i, pattern) = ws(pat)(i)?;
  let tail =
      match pattern {
          Pat::List(inner_list) => inner_list,
          _ => vec![pattern]
      };
  let (i, _) = ws(tag("]"))(i)?;

  let cons = [&head[..], &tail[..]].concat();
  Ok((i, crate::parser::ast::Pat::List(cons)))
}

fn vars(i: &str) -> IResult<&str, Vec<Var>> {
  alt((
    map(var,|o| vec![o]),
    comma_sep_list("<", ">", var)
  ))(i)
}

fn  let_in(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("let"))(i)?;
  // Delay thinking about how the vars and exprs match in the equal sign
  let (i, vars) = vars(i)?;
  let (i, _) = ws(tag("="))(i)?;
  let (i, exprs1) = exprs(i)?;
  let (i, _) = ws(tag("in"))(i)?;
  let (i, exprs2) = exprs(i)?;
  Ok((i, crate::parser::ast::Expr::Let(vars, vec![exprs1], vec![exprs2])))
}

fn alias(i: &str) -> IResult<&str, Pat> {
  map(tuple((var,ws(tag("=")),pat)),|(variable,_,pattern)| crate::parser::ast::Pat::Alias(variable, Box::new(pattern)))(i)
}

fn pat(i: &str) -> IResult<&str, Pat> {
  alt((
    map(var,crate::parser::ast::Pat::Var),
    map(lit, crate::parser::ast::Pat::Lit),
    pat_nested_list,
    map(comma_sep_list("[", "]", pat), crate::parser::ast::Pat::List),
    map(comma_sep_list("{", "}", pat), crate::parser::ast::Pat::Tuple),
    alias,
  ))(i)
}

fn pats(i: &str) -> IResult<&str, Vec<Pat>> {
  alt((
    map(pat,|o| vec![o]),
    comma_sep_list("<", ">", pat)
  ))(i)
}

fn clause(i: &str) -> IResult<&str, Clause> {
  let (i, pats) = pats(i)?;
  let (i, _) = ws(tag("when"))(i)?;
  let (i, exprs1) = map(exprs,|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag("->"))(i)?;
  let (i, exprs2) = map(exprs,|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Clause{pats, when: exprs1, res: exprs2}))
}

fn case_of(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("case"))(i)?;
  let (i, exprs) = map(exprs,|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag("of"))(i)?;
  let (i, clauses) = many0(clause)(i)?;
  let (i, _) = ws(tag("end"))(i)?;
  Ok((i, crate::parser::ast::Expr::Case(exprs, clauses)))
}

fn letrec(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("letrec"))(i)?;
  let (i, fundefs) = many0(ws(fun))(i)?;
  let (i, _) = ws(tag("in"))(i)?;
  let (i, expressions) = map(exprs,|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Expr::LetRec(fundefs, expressions)))
}

fn apply(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("apply"))(i)?;
  let (i, exprs0) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, exprs_args) = comma_sep_list("(", ")", exprs)(i)?;
  Ok((i, crate::parser::ast::Expr::Apply(exprs0, exprs_args)))
}

fn call(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("call"))(i)?;
  let (i, module) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag(":"))(i)?;
  let (i, name) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
  Ok((i, crate::parser::ast::Expr::Call(module, name, args)))
}

fn primop(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("primop"))(i)?;
  let (i, name) = ws(atom)(i)?;
  let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
  Ok((i, crate::parser::ast::Expr::PrimOp(name, args)))
}

fn receive(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("receive"))(i)?;
  let (i, clauses) = many0(clause)(i)?;
  let (i, _) = ws(tag("after"))(i)?;
  let (i, timeout) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag("->"))(i)?;
  let (i, action) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Expr::Receive(clauses, timeout, action)))
}

fn try_expr(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("try"))(i)?;
  let (i, arg) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag("of"))(i)?;
  let (i, vars_) = comma_sep_list("<", ">", var)(i)?;
  let (i, _) = ws(tag("->"))(i)?;
  let (i, body) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, _) = ws(tag("catch"))(i)?;
  let (i, evars) = comma_sep_list("<", ">", var)(i)?;
  let (i, _) = ws(tag("->"))(i)?;
  let (i, handler) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Expr::Try(arg, vars_, body, evars, handler)))
}

fn do_expr(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("do"))(i)?;
  let (i, exprs1) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  let (i, exprs2) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Expr::Do(exprs1, exprs2)))
}

fn catch(i: &str) -> IResult<&str, Expr> {
  let (i, _) = ws(tag("catch"))(i)?;
  let (i, exprs1) = map(ws(exprs),|o| vec![o])(i)?; // TODO: Fixme This is stupid
  Ok((i, crate::parser::ast::Expr::Catch(exprs1)))
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt((
        map(var, crate::parser::ast::Expr::Var),
        map(fname, crate::parser::ast::Expr::Fname),
        map(lit, crate::parser::ast::Expr::Lit),
        map(fun, |fun| crate::parser::ast::Expr::Fun(Box::new(fun))),
        expr_nested_list,
        map(comma_sep_list("[", "]", exprs), crate::parser::ast::Expr::List),
        map(comma_sep_list("{", "}", exprs), crate::parser::ast::Expr::Tuple),
        let_in,
        case_of,
        letrec,
        apply,
        call,
        primop,
        receive,
        try_expr,
        do_expr,
        catch,
    ))(i)
}

fn exprs(i: &str) -> IResult<&str, Expr> {
    alt((
        opt_annotation(expr),
        map(comma_sep_list("<", ">", expr),crate::parser::ast::Expr::List)
    ))(i)
}

fn fun(i: &str) -> IResult<&str, FunDef> {
    let (i, head) = fname(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, (args, exprs)) = opt_annotation(fun_inner)(i)?;

    Ok((i, FunDef{head, args, body: exprs}))
}

fn fun_inner(i: &str) -> IResult<&str, (Vec<Var>, Expr)> {
    let (i, _) = ws(tag("fun"))(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, args) = comma_sep_list("(", ")", var)(i)?;
    let (i, _) = ws(tag("->"))(i)?;

    let (i, exprs) = ws(exprs)(i)?;
    Ok((i, (args, exprs)))
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
