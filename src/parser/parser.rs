use nom::{IResult, sequence::{delimited, tuple, pair}, character::{complete::{multispace0, digit1}, is_digit}, multi::{separated_list0, many0, fold_many0}, combinator::{map_res, opt, value, map}, branch::alt, number::complete::float, error::{ParseError, ErrorKind}, Parser, InputTakeAtPosition, AsChar, InputIter, InputLength, Slice};
use crate::parser::ast::{Module, FunHead, Fname, Attribute, Atom, Const, Lit, Integer, FunDef, Expr, Var, Clause, Pat};
use nom::bytes::complete::{tag, take_until};
use crate::parser::ast::Const::List;
use crate::parser::parser::Lit::EmptyList;
use crate::parser::parser::Lit::Int;
use crate::parser::parser::Lit::Float;

use super::terminals::{atom, integer, char_, string, var};

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
