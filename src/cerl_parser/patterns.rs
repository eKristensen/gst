use std::rc::Rc;

use nom::{branch::alt, combinator::map, sequence::tuple, IResult};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{AnnoPat, AnnoVar, Pat},
    cinput::CInput,
    expressions::atomic_literal,
    helpers::{comma_sep_list, cons, loc, opt_annotation, wsa},
    tokeniser::var,
};

// WSA OK
// Note: Do not Rc as AnnoPat is used in Vec
pub fn anno_pattern(i: CInput) -> IResult<CInput, AnnoPat, ErrorTree<CInput>> {
    map(loc(opt_annotation(pattern)), |(loc, (inner, anno))| {
        AnnoPat { loc, anno, inner }
    })(i)
}

// WSA OK
fn pattern(i: CInput) -> IResult<CInput, Rc<Pat>, ErrorTree<CInput>> {
    map(
        alt((
            map(loc(atomic_literal), |(l, o)| Pat::Lit(l, o.into())),
            map(loc(tuple_pattern), |(l, o)| Pat::Tuple(l, o)),
            // Ready for extension: Map pattern
            map(loc(cons_pattern), |(l, o)| Pat::Cons(l, o)),
            // Ready for extension: Binary pattern
            map(loc(alias), |(l, (var, pat))| Pat::Alias(l, var, pat)),
            map(loc(anno_variable), |(l, o)| Pat::Var(l, o.into())),
        )),
        |o| o.into(),
    )(i)
}

// WSA OK
fn tuple_pattern(i: CInput) -> IResult<CInput, Vec<AnnoPat>, ErrorTree<CInput>> {
    comma_sep_list("{", "}", anno_pattern)(i)
}

// WSA OK
fn cons_pattern(i: CInput) -> IResult<CInput, Vec<AnnoPat>, ErrorTree<CInput>> {
    cons(anno_pattern)(i)
}

// WSA OK
// Note: Do not wrap in Rc as AnnoVar is also used directly in Vec
pub fn anno_variable(i: CInput) -> IResult<CInput, AnnoVar, ErrorTree<CInput>> {
    map(loc(opt_annotation(var)), |(loc, (name, anno))| AnnoVar {
        loc,
        anno,
        name,
    })(i)
}

// WSA OK
fn alias(i: CInput) -> IResult<CInput, (Rc<AnnoVar>, Rc<AnnoPat>), ErrorTree<CInput>> {
    map(
        tuple((anno_variable, wsa(tag("=")), anno_pattern)),
        |(variable, _, pattern)| (variable.into(), pattern.into()),
    )(i)
}
