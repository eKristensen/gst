use std::ops::RangeFrom;

use nom::{
    branch::alt,
    character::complete::{char, digit1, one_of},
    combinator::{map, map_res, opt, recognize, success, value},
    error::{ErrorKind, ParseError},
    multi::fold_many0,
    sequence::{delimited, tuple},
    AsChar, Err, IResult, InputIter, InputLength, InputTakeAtPosition, Parser, Slice,
};

use super::{
    ast::{Atom, Integer, Var},
    lex::{is_control, is_inputchar, is_uppercase, namechar},
};

// TODO: Check terminology: Is everything in here "terminals"?

fn octal(i: &str) -> IResult<&str, String> {
    let mut str:String = "".to_owned();
    let (i, out) = one_of("01234567")(i)?; // TODO: Use is_oct_digit or oct_digit1 instead?
    // TODO Rewrite. This is impossible to read!
    str.push(out);
    let (i, out) = opt(one_of("01234567"))(i)?;
    match out {
        Some(out) => {
            str.push(out);
            let (i, out) = opt(one_of("01234567"))(i)?;
            match out {
                Some(out) => {
                    str.push(out);
                    Ok((i,str))
                },
                None => Ok((i,str))
            }
        },
        None => Ok((i,str)),
    }
}

fn escapechar(i: &str) -> IResult<&str, char> {
    alt((
        char('b'), // escapechar
        char('d'),
        char('e'),
        char('f'),
        char('n'),
        char('r'),
        char('s'),
        char('t'),
        char('v'),
        char('\''),
        char('\\'),
    ))(i)
}

// Based on: https://github.com/rust-bakery/nom/blob/main/examples/string.rs
/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
pub fn parse_escaped(i: &str) -> IResult<&str, String>
{
    map(
        tuple((
            char('\\'),
            alt((
                octal,     // octal
                //hat_ctl_char, // ^ctlchar
                map(escapechar,|o| o.to_string()), // escapechar

            )),
        )), |(o1,o2)| format!("{}{}",o1,o2)
    )(i)
}

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
        map(parse_escaped, |o| o),
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
            alt((
                value(true, char('+')),
                (value(false, char('-'))),
                success(true),
            )),
            // Read integer value
            map_res(digit1, str::parse::<i64>),
        )),
        |(plus, i)| {
            if plus {
                Integer(i)
            } else {
                Integer(-i)
            }
        },
    )(i)
}

// The build in float function in nom is not enough. It can consume integers and present them as floats.
// We need to check the format before using nom's float function
pub fn float(i: &str) -> IResult<&str, f32> {
    // TODO: This is even worse than regex....
    let (i, float_str) = recognize(tuple((
        opt(alt((char('+'), char('-')))),
        digit1,
        char('.'),
        digit1,
        opt(tuple((
            alt((char('E'), char('e'))),
            opt(alt((char('+'), char('-')))),
            digit1,
        ))),
    )))(i)?;
    let (_, float_res) = nom::number::complete::float(float_str)?;
    Ok((i, float_res))
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
        map(parse_escaped, |o| o),
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

pub fn char_(i: &str) -> IResult<&str, String> {
    let (i, _) = char('$')(i)?;
    alt((map(char_name,|o| o.to_string()), parse_escaped))(i)
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
    use crate::parser::{ast::Lit, lex::lit};

    use super::*;

    #[test]
    fn test_integer_literals() {
        assert_eq!(integer("8"), Ok(("", Integer(8))));
        assert_eq!(integer("+17"), Ok(("", Integer(17))));
        assert_eq!(integer("299792458"), Ok(("", Integer(299792458))));
        assert_eq!(integer("-4711"), Ok(("", Integer(-4711))));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(lit("8"), Ok(("", Lit::Int(Integer(8)))));
        assert_eq!(lit("+17"), Ok(("", Lit::Int(Integer(17)))));
        assert_eq!(lit("299792458"), Ok(("", Lit::Int(Integer(299792458)))));
        assert_eq!(lit("-4711"), Ok(("", Lit::Int(Integer(-4711)))));
        
        // Mindless sanity check
        assert_ne!(lit("8"), Ok(("", Lit::Int(Integer(42)))));

        // TODO: Negative / Expect Error test
    }

    // TODO: Could not get direct test on float to work, using lit from lex.rs
    #[test]
    fn test_floating_point_numbers() {
        assert_eq!(float("0.0"), Ok(("", 0.0)));
        assert_eq!(float("2.7182818"), Ok(("", 2.7182818)));
        assert_eq!(float("-3.14"), Ok(("", -3.14)));
        assert_eq!(float("+1.2E-6"), Ok(("", 1.2e-6)));
        assert_eq!(float("-1.23e12"), Ok(("", -1.23e12)));
        assert_eq!(float("1.0e+9"), Ok(("", 1.0e9)));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(lit("0.0"), Ok(("", Lit::Float(0.0))));
        assert_eq!(lit("2.7182818"), Ok(("", Lit::Float(2.7182818))));
        assert_eq!(lit("-3.14"), Ok(("", Lit::Float(-3.14))));
        assert_eq!(lit("+1.2E-6"), Ok(("", Lit::Float(1.2e-6))));
        assert_eq!(lit("-1.23e12"), Ok(("", Lit::Float(-1.23e12))));
        assert_eq!(lit("1.0e+9"), Ok(("", Lit::Float(1.0e9))));

        // Mindless sanity check
        assert_ne!(float("0.0"), Ok(("", 2.0)));

        // TODO: Negative / Expect Error test

    }

    #[test]
    fn test_atom() {
        assert_eq!(atom("'foo'"), Ok(("", Atom("foo".to_owned()))));
        assert_eq!(atom("'Bar'"), Ok(("", Atom("Bar".to_owned()))));
        assert_eq!(atom("'foo bar'"), Ok(("", Atom("foo bar".to_owned()))));
        assert_eq!(atom("''"), Ok(("", Atom("".to_owned()))));

        // TODO: Is the test correct with \\ == \ in the string?
        assert_eq!(octal("012"), Ok(("", "012".to_owned())));
        assert_eq!(parse_escaped("\\011"), Ok(("", "\\011".to_owned())));
        assert_eq!(atom("'\\010'"), Ok(("", Atom("\\010".to_owned()))));
        // Literal output expected "%#\010@\n!"
        assert_eq!(atom("'%#\\010@\\n!'"), Ok(("", Atom("%#\\010@\\n!".to_owned()))));
        
        assert_eq!(atom("'_hello_world'"), Ok(("", Atom("_hello_world".to_owned()))));
        assert_eq!(atom("'=:='"), Ok(("", Atom("=:=".to_owned()))));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(lit("'foo'"), Ok(("", Lit::Atom(Atom("foo".to_owned())))));
        assert_eq!(lit("'Bar'"), Ok(("", Lit::Atom(Atom("Bar".to_owned())))));
        assert_eq!(lit("'foo bar'"), Ok(("", Lit::Atom(Atom("foo bar".to_owned())))));
        assert_eq!(lit("''"), Ok(("", Lit::Atom(Atom("".to_owned())))));
        assert_eq!(lit("'%#\\010@\\n!'"), Ok(("", Lit::Atom(Atom("%#\\010@\\n!".to_owned())))));
        assert_eq!(lit("'_hello_world'"), Ok(("", Lit::Atom(Atom("_hello_world".to_owned())))));
        assert_eq!(lit("'=:='"), Ok(("", Lit::Atom(Atom("=:=".to_owned())))));

        // Mindless sanity check
        assert_ne!(atom("'foo'"), Ok(("", Atom("bar".to_owned()))));
    }
}
