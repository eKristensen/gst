use nom::{branch::alt, combinator::map, sequence::tuple, IResult};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Pat, PatInner},
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::lit,
    terminals::var,
};

// TODO: Common pattern for nested list, avoid manual rewrite!
fn pat_list(i: &str) -> IResult<&str, PatInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, head) = ws(pat)(i)?;
    let (i, _) = ws(tag("|"))(i)?;
    let (i, tail) = ws(pat)(i)?;
    let (i, _) = ws(tag("]"))(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::PatInner::Cons(Box::new((head, tail))),
    ))
}

fn alias(i: &str) -> IResult<&str, PatInner, ErrorTree<&str>> {
    map(tuple((var, ws(tag("=")), pat)), |(variable, _, pattern)| {
        crate::cerl_parser::ast::PatInner::Alias(variable, Box::new(pattern))
    })(i)
}

fn pat(i: &str) -> IResult<&str, Pat, ErrorTree<&str>> {
    map(opt_annotation(pat_inner), |(inner, anno)| Pat {
        anno,
        inner,
    })(i)
}

fn pat_inner(i: &str) -> IResult<&str, PatInner, ErrorTree<&str>> {
    alt((
        map(var, crate::cerl_parser::ast::PatInner::Var),
        map(lit, crate::cerl_parser::ast::PatInner::Lit),
        pat_list,
        map(
            comma_sep_list("{", "}", pat),
            crate::cerl_parser::ast::PatInner::Tuple,
        ),
        alias,
    ))(i)
}

pub fn pats(i: &str) -> IResult<&str, Vec<Pat>, ErrorTree<&str>> {
    alt((map(pat, |o| vec![o]), comma_sep_list("<", ">", pat)))(i)
}
