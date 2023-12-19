use std::ops::RangeFrom;

use nom::{
    branch::alt,
    character::complete::{char, digit1},
    combinator::{map, map_res, opt, value, success},
    error::{ErrorKind, ParseError},
    multi::fold_many0,
    sequence::{delimited, tuple},
    AsChar, Err, IResult, InputIter, InputLength, InputTakeAtPosition, Parser, Slice,
};

use super::{
    ast::{Atom, Integer, Var},
    lex::{is_control, is_inputchar, is_uppercase, namechar, parse_escaped_char},
};

fn parse_atom_input_chr<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let item_chr = item.as_char() as u8; // TODO extend trait instead of this
            !(is_inputchar(item_chr)) // With trait it could be item.is_inputchar()
        ||  is_control(item_chr)
        ||  item_chr == 0x5C // Hex codes are not easy to read...
        ||  item_chr == 0x27
        },
        nom::error::ErrorKind::Fix, // TODO: Actual error message
    )
}

// TODO Can it this be integrated into atom somehow?
fn parse_atom_fragments(i: &str) -> IResult<&str, String> {
    alt((
        map(parse_atom_input_chr, |o: &str| o.to_string()),
        map(parse_escaped_char, |o| o.to_string()),
    ))(i)
}

// TODO: Accept atoms with escaped chars
// Inspired by :
// - https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
// - https://github.com/rust-bakery/nom/blob/main/examples/string.rs
pub fn atom(i: &str) -> IResult<&str, Atom> {
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        parse_atom_fragments,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            string.push_str(&fragment);
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    map(delimited(char('\''), build_string, char('\'')), |o| {
        Atom(o.to_string())
    })
    .parse(i)
}

pub fn integer(i: &str) -> IResult<&str, Integer> {
    map(
        tuple((
            // Consume + or - or nothing and save the sign
            alt((value(true,char('+')), (value(false,char('-'))), success(true))),
            // Read integer value
            map_res(digit1, str::parse::<i64>))
        ), |(plus ,i)| {if plus { Integer(i) } else { Integer(-i) }})(i)
}

fn parse_string_input_chr<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let item_chr = item.as_char() as u8; // TODO extend trait instead of this
            !(is_inputchar(item_chr)) // With trait it could be item.is_inputchar()
        ||  is_control(item_chr)
        ||  item_chr == 0x5C // Hex codes are not easy to read...
        ||  item_chr == 0x22
        },
        nom::error::ErrorKind::Fix, // TODO: Actual error message
    )
}

// TODO Can it this be integrated into string somehow?
fn parse_string_fragments(i: &str) -> IResult<&str, String> {
    alt((
        map(parse_string_input_chr, |o: &str| o.to_string()),
        map(parse_escaped_char, |o| o.to_string()),
    ))(i)
}

// TODO: Deduplicate, a lot like fn atom
pub fn string(i: &str) -> IResult<&str, String> {
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        parse_string_fragments,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            string.push_str(&fragment);
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    map(delimited(char('"'), build_string, char('"')), |o| {
        o.to_string()
    })
    .parse(i)
}

pub fn char_(i: &str) -> IResult<&str, char> {
    let (i, _) = char('$')(i)?;
    alt((char_name, parse_escaped_char))(i)
}

fn char_name<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
where
    T: InputIter + InputLength + Slice<RangeFrom<usize>>,
    <T as InputIter>::Item: AsChar,
{
    let mut it = input.iter_indices();
    let (input, candidate) = match it.next() {
        None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
        Some((_, c)) => match it.next() {
            None => Ok((input.slice(input.input_len()..), c.as_char())),
            Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
        },
    }?;
    let item_chr = candidate.as_char() as u8; // TODO extend trait instead of this
    if !(is_inputchar(item_chr)) // With trait it could be item.is_inputchar()
    ||  is_control(item_chr)
    ||  item_chr == 0x20 // Hex codes are not easy to read...
    ||  item_chr == 0x5C
    {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
    }
    Ok((input, candidate))
}

// TODO: There must exist a easier way to do this
fn uppercase_char<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
where
    T: InputIter + InputLength + Slice<RangeFrom<usize>>,
    <T as InputIter>::Item: AsChar,
{
    let mut it = input.iter_indices();
    let (input, candidate) = match it.next() {
        None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
        Some((_, c)) => match it.next() {
            None => Ok((input.slice(input.input_len()..), c.as_char())),
            Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
        },
    }?;
    let item_chr = candidate.as_char() as u8; // TODO extend trait instead of this
    if !(is_uppercase(item_chr))
    // With trait it could be item.is_inputchar()
    {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
    }
    Ok((input, candidate))
}

pub fn var(i: &str) -> IResult<&str, Var> {
    let (i, var_name_head) = alt((
        map(uppercase_char, |o: char| o.to_string()),
        map(char('_'), |o| o.to_string()), // TODO: Odd but elc accepts "_" as a valid variable name despite it explicitly being invalid in the core erlang specification
    ))(i)?;
    let (i, var_name_tail) = fold_many0(
        // Our parser function– parses a single string fragment
        namechar,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            string.push(fragment);
            string
        },
    )(i)?;
    // TODO: A bit much string manipulation, maybe inefficient...
    let mut final_var_name = var_name_head;
    final_var_name.push_str(&var_name_tail);
    Ok((i, Var(final_var_name)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_literals() {
        assert_eq!(integer("8"), Ok(("", Integer(8))));
        assert_eq!(integer("+17"), Ok(("", Integer(17))));
        assert_eq!(integer("299792458"), Ok(("", Integer(299792458))));
        assert_eq!(integer("-4711"), Ok(("", Integer(-4711))));
        // assert_eq!(integer("abcd\tefg"), Ok(("\tefg", "abcd")));
        // assert_eq!(integer(" abcdefg"), Err(nom::Err::Error((" abcdefg", nom::error::ErrorKind::IsNot))));
    }

}

