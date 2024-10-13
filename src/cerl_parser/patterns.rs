use std::rc::Rc;

use nom::{branch::alt, combinator::map, sequence::tuple, IResult};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{AnnoPat, AnnoVar, Pat},
    expressions::atomic_literal,
    helpers::{comma_sep_list, cons, opt_annotation, wsa},
    tokeniser::var,
};

// WSA OK
// Note: Do not Rc as AnnoPat is used in Vec
pub fn anno_pattern(i: &str) -> IResult<&str, AnnoPat, ErrorTree<&str>> {
    map(opt_annotation(pattern), |(inner, anno)| AnnoPat {
        anno,
        inner,
    })(i)
}

// WSA OK
fn pattern(i: &str) -> IResult<&str, Rc<Pat>, ErrorTree<&str>> {
    map(
        alt((
            map(atomic_literal, |o| Pat::Lit(o.into())),
            map(tuple_pattern, Pat::Tuple),
            // Ready for extension: Map pattern
            map(cons_pattern, Pat::Cons),
            // Ready for extension: Binary pattern
            map(alias, |(var, pat)| Pat::Alias(var, pat)),
            map(anno_variable, |o| Pat::Var(o.into())),
        )),
        |o| o.into(),
    )(i)
}

// WSA OK
fn tuple_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    comma_sep_list("{", "}", anno_pattern)(i)
}

// WSA OK
fn cons_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    cons(anno_pattern)(i)
}

// WSA OK
// Note: Do not wrap in Rc as AnnoVar is also used directly in Vec
pub fn anno_variable(i: &str) -> IResult<&str, AnnoVar, ErrorTree<&str>> {
    map(opt_annotation(var), |(name, anno)| AnnoVar { anno, name })(i)
}

// WSA OK
fn alias(i: &str) -> IResult<&str, (Rc<AnnoVar>, Rc<AnnoPat>), ErrorTree<&str>> {
    map(
        tuple((anno_variable, wsa(tag("=")), anno_pattern)),
        |(variable, _, pattern)| (variable.into(), pattern.into()),
    )(i)
}
