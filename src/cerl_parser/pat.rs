use nom::{branch::alt, bytes::complete::tag, combinator::map, sequence::tuple, IResult};

use super::{
    ast::Pat,
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::lit,
    terminals::var,
};

// TODO: Common pattern for nested list, avoid manual rewrite!
fn pat_nested_list(i: &str) -> IResult<&str, Pat> {
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

fn alias(i: &str) -> IResult<&str, Pat> {
    map(tuple((var, ws(tag("=")), pat)), |(variable, _, pattern)| {
        crate::cerl_parser::ast::Pat::Alias(variable, Box::new(pattern))
    })(i)
}

fn pat(i: &str) -> IResult<&str, Pat> {
    opt_annotation(pat_inner)(i)
}

fn pat_inner(i: &str) -> IResult<&str, Pat> {
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

pub fn pats(i: &str) -> IResult<&str, Vec<Pat>> {
    alt((map(pat, |o| vec![o]), comma_sep_list("<", ">", pat)))(i)
}
