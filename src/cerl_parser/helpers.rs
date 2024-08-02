use nom::{
    branch::alt,
    bytes::complete::take_until,
    character::complete::multispace0,
    combinator::map,
    multi::{many0, separated_list0},
    sequence::{delimited, tuple},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{ast::Anno, constants::constant};

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#-ceol-style-comments
// Comment
fn comment(i: &str) -> IResult<&str, (), ErrorTree<&str>> {
    // TODO: Better to use char('%') instead of tag("%") ??
    map(
        tuple((
            nom::character::complete::char('%'),
            take_until("\n"),
            tag("\n"),
        )),
        |_| (),
    )(i) // TODO: Better to use end of line instead of is_not ? TODO: Type of line endings...
}

// Parse annotation with custom parser for the inner part
fn annotation<'a, F, O>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, (O, Anno), ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>>,
{
    map(
        tuple((
            ws(tag("(")),
            inner,
            ws(tag("-|")),
            separated_list0(ws(tag(",")), ws(constant)),
            ws(tag(")")),
        )),
        |(_, inner_out, _, anno, _)| (inner_out, Anno(Some(anno))),
    )
}

// Generic to allow a parser to be wrapped in annotation.
pub fn opt_annotation<'a, F, O>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, (O, Anno), ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>> + Clone,
{
    alt((map(inner.clone(), |o| (o, Anno(None))), annotation(inner)))
}

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#wrapper-combinators-that-eat-whitespace-before-and-after-a-parser
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
// TODO: Stupid comment implementation maybe something better exists?
pub fn ws<'a, F, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>>,
    &'a str: nom::InputLength + nom::InputTakeAtPosition + Clone,
    <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
    <&'a str as nom::InputTakeAtPosition>::Item: Clone,
{
    map(
        tuple((
            multispace0,
            many0(comment),
            multispace0,
            inner,
            multispace0,
            many0(comment),
            multispace0,
        )),
        |(_, _, _, o, _, _, _)| o,
    )
}

// General helper for comma-separated lists
pub fn comma_sep_list<'a, O, F>(
    start: &'static str, // TODO: Static not great, but idk what else to do now
    end: &'static str,   // TODO: Static lifetime not great
    elements: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>>,
{
    delimited(
        ws(tag(start)),
        separated_list0(ws(tag(",")), ws(elements)),
        ws(tag(end)),
    )
}

// General helper for cons lists with different inner parsers
// Note: [A,B] == [A|[B]] - See Core Erlang Spec v 1.0.3 section 5.4
pub fn cons<'a, O, F>(
    elements: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>> + Clone,
{
    alt((
        ws(comma_sep_list("[", "]", elements.clone())),
        map(
            tuple((
                ws(tag("[")),
                elements.clone(),
                ws(tag("|")),
                elements,
                ws(tag("]")),
            )),
            |(_, head, _, tail, _)| vec![head, tail],
        ),
    ))
}

// General helper for when elements may be in an angle bracket list.
pub fn opt_angle_bracket<'a, O, F>(
    elements: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>> + Clone,
{
    alt((
        map(ws(elements.clone()), |single_element| vec![single_element]),
        ws(comma_sep_list("<", ">", elements)),
    ))
}
