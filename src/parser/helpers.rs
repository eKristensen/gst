use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::multispace0,
    combinator::{map, opt},
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, pair, tuple},
    IResult, Parser,
};

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#-ceol-style-comments
// Comment
fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    // TODO: Better to use char('%') instead of tag("%") ??
    map(
        pair(nom::character::complete::char('%'), take_until("\n")),
        |(_, _)| (),
    )(i) // TODO: Better to use end of line instead of is_not ? TODO: Type of line endings...
}

// Removes annotation
fn annotation<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
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
        )),
        |(_, o, _, _, _)| o,
    )
}

pub fn opt_annotation<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E> + Clone,
{
    alt((inner.clone(), annotation(inner)))
}

// Based on: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#wrapper-combinators-that-eat-whitespace-before-and-after-a-parser
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
// TODO: Stupid comment implementation maybe something better exists?
pub fn ws<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    &'a str: nom::InputLength + nom::InputTakeAtPosition + Clone,
    <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
    <&'a str as nom::InputTakeAtPosition>::Item: Clone,
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
        |(_, _, _, o, _, _, _)| o,
    )
}

// General helper for comma-separated lists
// TODO: Maybe too general? Some lists may require a least one element... e.g. is expr "<>" allowed? is "{}" allowed?
pub fn comma_sep_list<'a, O, E: ParseError<&'a str>, F>(
    start: &'a str,
    end: &'a str,
    elements: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(tag(start), separated_list0(tag(","), elements), tag(end))
}
