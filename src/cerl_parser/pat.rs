use nom::{
    branch::alt, bytes::complete::tag, combinator::map, error::ParseError, sequence::tuple, IResult,
};

use super::{
    ast::Pat,
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::lit,
    terminals::var,
};

// TODO: Common pattern for nested list, avoid manual rewrite!
fn pat_nested_list<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Pat, E> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, pattern) = ws(pat)(i)?;
    let head = match pattern {
        Pat::Cons(inner_list) => inner_list,
        _ => vec![pattern],
    };

    let (i, _) = ws(tag("|"))(i)?;
    let (i, pattern) = ws(pat)(i)?;
    let tail = match pattern {
        Pat::Cons(inner_list) => inner_list,
        _ => vec![pattern],
    };
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::cerl_parser::ast::Pat::Cons(cons)))
}

fn alias<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Pat, E> {
    map(tuple((var, ws(tag("=")), pat)), |(variable, _, pattern)| {
        crate::cerl_parser::ast::Pat::Alias(variable, Box::new(pattern))
    })(i)
}

fn pat<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Pat, E> {
    opt_annotation(pat_inner)(i)
}

fn pat_inner<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Pat, E> {
    alt((
        map(var, crate::cerl_parser::ast::Pat::Var),
        map(lit, crate::cerl_parser::ast::Pat::Lit),
        pat_nested_list,
        map(
            comma_sep_list("[", "]", pat),
            crate::cerl_parser::ast::Pat::Cons,
        ),
        map(
            comma_sep_list("{", "}", pat),
            crate::cerl_parser::ast::Pat::Tuple,
        ),
        alias,
    ))(i)
}

pub fn pats<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Vec<Pat>, E> {
    alt((map(pat, |o| vec![o]), comma_sep_list("<", ">", pat)))(i)
}
