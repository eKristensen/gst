use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::take_until,
    character::complete::multispace0,
    combinator::{map, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, tuple},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Anno, CLoc},
    cinput::CInput,
    constants::constant,
};

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#-ceol-style-comments
// Comment
fn comment(i: CInput) -> IResult<CInput, (), ErrorTree<CInput>> {
    map(
        tuple((
            nom::character::complete::char('%'),
            take_until("\n"),
            tag("\n"),
        )),
        |_| (),
    )(i)
}

// Parse annotation with custom parser for the inner part
// WSA OK (if inner WSA OK)
fn annotation<'a, F, O>(
    inner: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, (O, Rc<Anno>), ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>>,
{
    // We assume the caller has taken care of whitespace on top of input.
    map(
        tuple((
            wsa(tag("(")),
            inner, // inner will take care of whitespace after
            wsa(tag("-|")),
            delimited(
                wsa(tag("[")),
                separated_list0(wsa(tag(",")), loc(constant)),
                wsa(tag("]")),
            ),
            wsa(tag(")")),
        )),
        |(_, inner_out, _, anno, _)| (inner_out, Anno(Some(anno)).into()),
    )
}

// Generic to allow a parser to be wrapped in annotation.
// WSA OK (if inner WSA OK)
pub fn opt_annotation<'a, F, O>(
    inner: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, (O, Rc<Anno>), ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>> + Clone,
{
    // We assume the caller has taken care of whitespace on top of input.
    alt((
        map(inner.clone(), |o| (o, Anno(None).into())),
        annotation(inner),
    ))
}

// Consumes whitespace and any comments.
// The parser has no effect if there are no whitespace to consume.
pub fn wsc(i: CInput) -> IResult<CInput, (), ErrorTree<CInput>> {
    value((), tuple((multispace0, many0(comment), multispace0)))(i)
}

// Run internal parser and remove whitespace after
pub fn wsa<'a, F, O>(
    inner: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, O, ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>>,
    &'a str: nom::InputLength + nom::InputTakeAtPosition + Clone,
    <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
    <&'a str as nom::InputTakeAtPosition>::Item: Clone,
{
    map(pair(inner, wsc), |(o, _)| o)
}

// General helper for comma-separated lists
// WSA OK (if inner WSA OK)
pub fn comma_sep_list<'a, O, F>(
    start: &'static str, // TODO: Static not great, but idk what else to do now
    end: &'static str,   // TODO: Static lifetime not great
    elements: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, Vec<O>, ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>>,
{
    delimited(
        wsa(tag(start)),
        separated_list0(wsa(tag(",")), elements),
        wsa(tag(end)),
    )
}

// General helper for cons lists with different inner parsers
// Note: [A,B] == [A|[B]] - See Core Erlang Spec v 1.0.3 section 5.4
// WSA OK (if inner WSA OK)
pub fn cons<'a, O, F>(
    elements: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, Vec<O>, ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>> + Clone,
{
    alt((
        comma_sep_list("[", "]", elements.clone()),
        map(
            tuple((
                wsa(tag("[")),
                elements.clone(),
                wsa(tag("|")),
                elements,
                wsa(tag("]")),
            )),
            |(_, head, _, tail, _)| vec![head, tail],
        ),
    ))
}

// General helper for when elements may be in an angle bracket list.
// WSA OK (if inner WSA OK)
pub fn opt_angle_bracket<'a, O, F>(
    elements: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, Vec<O>, ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>> + Clone,
{
    alt((
        map(elements.clone(), |single_element| vec![single_element]),
        comma_sep_list("<", ">", elements),
    ))
}

// General helper for adding basic location information to parsed statements.
pub fn loc<'a, O, F>(
    mut inner: F,
) -> impl FnMut(CInput<'a>) -> IResult<CInput<'a>, (CLoc, O), ErrorTree<CInput<'a>>>
where
    F: Parser<CInput<'a>, O, ErrorTree<CInput<'a>>>,
{
    move |i: CInput| {
        let start = i.get_loc();
        inner.parse(i).map(|(i, o)| {
            let end = i.get_loc();
            let cloc = CLoc {
                comment: None,
                start,
                end,
            };

            (i, (cloc, o))
        })
    }
}
