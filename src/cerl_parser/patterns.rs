use nom::{branch::alt, combinator::map, sequence::tuple, IResult};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{AnnoPat, AnnoVar, Pat},
    expressions::atomic_literal,
    helpers::{comma_sep_list, opt_annotation, ws},
    tokeniser::var,
};

pub fn anno_pattern(i: &str) -> IResult<&str, AnnoPat, ErrorTree<&str>> {
    map(opt_annotation(pattern), |(inner, anno)| AnnoPat {
        anno,
        inner,
    })(i)
}

fn pattern(i: &str) -> IResult<&str, Pat, ErrorTree<&str>> {
    alt((
        map(atomic_literal, Pat::Lit),
        map(tuple_pattern, Pat::Tuple),
        // Ready for extension: Map pattern
        map(cons_pattern, Pat::Cons),
        // Ready for extension: Binary pattern
        map(alias, Pat::Alias),
        map(anno_variable, Pat::Var),
    ))(i)
}

fn tuple_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    ws(comma_sep_list("{", "}", anno_pattern))(i)
}

// TODO: Make cons general and move general cons comment there.
// Note: [A,B] == [A|[B]] - See Core Erlang Spec v 1.0.3 section 5.4
fn cons_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    alt((
        ws(comma_sep_list("[", "]", anno_pattern)),
        map(
            tuple((
                ws(tag("[")),
                anno_pattern,
                ws(tag("|")),
                anno_pattern,
                ws(tag("]")),
            )),
            |(_, head, _, tail, _)| vec![head, tail],
        ),
    ))(i)
}

fn anno_variable(i: &str) -> IResult<&str, AnnoVar, ErrorTree<&str>> {
    map(opt_annotation(var), |(name, anno)| AnnoVar { anno, name })(i)
}

fn alias(i: &str) -> IResult<&str, (AnnoVar, Box<AnnoPat>), ErrorTree<&str>> {
    map(
        tuple((anno_variable, ws(tag("=")), anno_pattern)),
        |(variable, _, pattern)| Pat::Alias(variable, Box::new(pattern)),
    )(i)
}
