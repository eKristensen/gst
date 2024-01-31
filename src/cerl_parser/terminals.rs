use std::ops::RangeFrom;

use nom::{
    branch::alt,
    character::complete::{char, digit1, one_of},
    combinator::{map, map_res, opt, recognize, success, value},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, fold_many1},
    sequence::{delimited, tuple},
    AsChar, Err, IResult, InputIter, InputLength, InputTakeAtPosition, Parser, Slice,
};
use nom_supreme::error::ErrorTree;

use super::{
    ast::{Atom, Var},
    helpers::{opt_annotation, ws},
    lex::{is_control, is_ctlchar, is_inputchar, is_uppercase, namechar},
};

// TODO: Check terminology: Is everything in here "terminals"?

fn octal(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    let mut str: String = "".to_owned();
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
                    Ok((i, str))
                }
                None => Ok((i, str)),
            }
        }
        None => Ok((i, str)),
    }
}

fn escapechar(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
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

fn hat_ctlchar(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    map(tuple((char('^'), parse_ctlchar)), |(o1, o2)| {
        format!("{}{}", o1, o2)
    })(i)
}

// Based on: https://github.com/rust-bakery/nom/blob/main/examples/string.rs
/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
pub fn parse_escaped(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    map(
        tuple((
            char('\\'),
            alt((
                octal,                              // octal
                hat_ctlchar,                        // ^ctlchar
                map(escapechar, |o| o.to_string()), // escapechar
            )),
        )),
        |(o1, o2)| format!("{}{}", o1, o2),
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
fn parse_atom_fragments(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    alt((
        map(parse_atom_input_chr, |o: &str| o.to_string()),
        map(parse_escaped, |o| o),
    ))(i)
}

// Note: Apparently atoms can be annotated as well
pub fn atom(i: &str) -> IResult<&str, Atom, ErrorTree<&str>> {
    opt_annotation(atom_inner)(i)
}

// TODO: Accept atoms with escaped chars
// Inspired by :
// - https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
// - https://github.com/rust-bakery/nom/blob/main/examples/string.rs
fn atom_inner(i: &str) -> IResult<&str, Atom, ErrorTree<&str>> {
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

pub fn integer<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, i64, E> {
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
                i
            } else {
                -i
            }
        },
    )(i)
}

// The build in float function in nom is not enough. It can consume integers and present them as floats.
// We need to check the format before using nom's float function
pub fn float<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, f32, E> {
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

fn parse_ctlchar<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let item_chr = item.as_char() as u8; // TODO extend trait instead of this
            !(is_ctlchar(item_chr)) // With trait it could be item.is_ctlchar()
        },
        nom::error::ErrorKind::Fix, // TODO: Actual error message
    )
}

// TODO Can it this be integrated into string somehow?
fn parse_string_fragments(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    alt((
        map(parse_string_input_chr, |o: &str| o.to_string()),
        map(parse_escaped, |o| o),
    ))(i)
}

// TODO: Deduplicate, a lot like fn atom
fn string_quoted(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
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

pub fn string(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    fold_many1(ws(string_quoted), String::new, |mut string, fragment| {
        string.push_str(&fragment);
        string
    })(i)
}

pub fn char_(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    let (i, _) = char('$')(i)?;
    alt((map(char_name, |o| o.to_string()), parse_escaped))(i)
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

// TODO: Test var annotation works as intended
pub fn var(i: &str) -> IResult<&str, Var, ErrorTree<&str>> {
    opt_annotation(var_inner)(i)
}

fn var_inner<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, Var, E> {
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
    use crate::cerl_parser::{ast::Lit, lex::lit};

    use super::*;

    #[test]
    fn test_integer_literals() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(integer::<()>("8"), Ok(("", 8)));
        assert_eq!(integer::<()>("+17"), Ok(("", 17)));
        assert_eq!(integer::<()>("299792458"), Ok(("", 299792458)));
        assert_eq!(integer::<()>("-4711"), Ok(("", -4711)));

        // TODO Move "lit" tests to lex.rs ?
        // TODO: Somehow make "::<()>" optional instead of having to type it...
        assert_eq!(lit("8").unwrap(), ("", Lit::Int(8)));
        assert_eq!(lit("+17").unwrap(), ("", Lit::Int(17)));
        assert_eq!(lit("299792458").unwrap(), ("", Lit::Int(299792458)));
        assert_eq!(lit("-4711").unwrap(), ("", Lit::Int(-4711)));

        // Mindless sanity check
        assert_ne!(lit("8").unwrap(), ("", Lit::Int(42)));

        // TODO: Negative / Expect Error test
    }

    // TODO: Could not get direct test on float to work, using lit from lex.rs
    #[test]
    fn test_floating_point_numbers() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(float::<()>("0.0"), Ok(("", 0.0)));
        assert_eq!(float::<()>("2.7182818"), Ok(("", 2.7182818)));
        assert_eq!(float::<()>("-3.14"), Ok(("", -3.14)));
        assert_eq!(float::<()>("+1.2E-6"), Ok(("", 1.2e-6)));
        assert_eq!(float::<()>("-1.23e12"), Ok(("", -1.23e12)));
        assert_eq!(float::<()>("1.0e+9"), Ok(("", 1.0e9)));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(lit("0.0").unwrap(), ("", Lit::Float(0.0)));
        assert_eq!(lit("2.7182818").unwrap(), ("", Lit::Float(2.7182818)));
        assert_eq!(lit("-3.14").unwrap(), ("", Lit::Float(-3.14)));
        assert_eq!(lit("+1.2E-6").unwrap(), ("", Lit::Float(1.2e-6)));
        assert_eq!(lit("-1.23e12").unwrap(), ("", Lit::Float(-1.23e12)));
        assert_eq!(lit("1.0e+9").unwrap(), ("", Lit::Float(1.0e9)));

        // Mindless sanity check
        assert_ne!(float::<()>("0.0"), Ok(("", 2.0)));

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_atom() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(atom("'foo'").unwrap(), ("", Atom("foo".to_owned())));
        assert_eq!(atom("'Bar'").unwrap(), ("", Atom("Bar".to_owned())));
        assert_eq!(atom("'foo bar'").unwrap(), ("", Atom("foo bar".to_owned())));
        assert_eq!(atom("''").unwrap(), ("", Atom("".to_owned())));

        // TODO: Is the test correct with \\ == \ in the string?
        assert_eq!(octal("012").unwrap(), ("", "012".to_owned()));
        assert_eq!(parse_escaped("\\011").unwrap(), ("", "\\011".to_owned()));
        assert_eq!(atom("'\\010'").unwrap(), ("", Atom("\\010".to_owned())));
        // Literal output expected "%#\010@\n!"
        assert_eq!(
            atom("'%#\\010@\\n!'").unwrap(),
            ("", Atom("%#\\010@\\n!".to_owned()))
        );

        assert_eq!(
            atom("'_hello_world'").unwrap(),
            ("", Atom("_hello_world".to_owned()))
        );
        assert_eq!(atom("'=:='").unwrap(), ("", Atom("=:=".to_owned())));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            lit("'foo'").unwrap(),
            ("", Lit::Atom(Atom("foo".to_owned())))
        );
        assert_eq!(
            lit("'Bar'").unwrap(),
            ("", Lit::Atom(Atom("Bar".to_owned())))
        );
        assert_eq!(
            lit("'foo bar'").unwrap(),
            ("", Lit::Atom(Atom("foo bar".to_owned())))
        );
        assert_eq!(lit("''").unwrap(), ("", Lit::Atom(Atom("".to_owned()))));
        assert_eq!(
            lit("'%#\\010@\\n!'").unwrap(),
            ("", Lit::Atom(Atom("%#\\010@\\n!".to_owned())))
        );
        assert_eq!(
            lit("'_hello_world'").unwrap(),
            ("", Lit::Atom(Atom("_hello_world".to_owned())))
        );
        assert_eq!(
            lit("'=:='").unwrap(),
            ("", Lit::Atom(Atom("=:=".to_owned())))
        );

        // Mindless sanity check
        assert_ne!(atom("'foo'").unwrap(), ("", Atom("bar".to_owned())));
    }

    #[test]
    fn test_char_literal() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(char_("$A").unwrap(), ("", "A".to_owned()));
        assert_eq!(char_("$$").unwrap(), ("", "$".to_owned()));
        assert_eq!(char_("$\\n").unwrap(), ("", "\\n".to_owned()));
        assert_eq!(char_("$\\s").unwrap(), ("", "\\s".to_owned()));
        assert_eq!(char_("$\\\\").unwrap(), ("", "\\\\".to_owned()));
        assert_eq!(char_("$\\12").unwrap(), ("", "\\12".to_owned()));
        assert_eq!(char_("$\\101").unwrap(), ("", "\\101".to_owned()));
        assert_eq!(char_("$\\^A").unwrap(), ("", "\\^A".to_owned()));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(lit("$A").unwrap(), ("", Lit::Char("A".to_owned())));
        assert_eq!(lit("$$").unwrap(), ("", Lit::Char("$".to_owned())));
        assert_eq!(lit("$\\n").unwrap(), ("", Lit::Char("\\n".to_owned())));
        assert_eq!(lit("$\\s").unwrap(), ("", Lit::Char("\\s".to_owned())));
        assert_eq!(lit("$\\\\").unwrap(), ("", Lit::Char("\\\\".to_owned())));
        assert_eq!(lit("$\\12").unwrap(), ("", Lit::Char("\\12".to_owned())));
        assert_eq!(lit("$\\101").unwrap(), ("", Lit::Char("\\101".to_owned())));
        assert_eq!(lit("$\\^A").unwrap(), ("", Lit::Char("\\^A".to_owned())));

        // Mindless sanity check
        assert_ne!(char_("$A").unwrap(), ("", "B".to_owned()));

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_strings() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            string("\"Hello, World!\"").unwrap(),
            ("", "Hello, World!".to_owned())
        );
        assert_eq!(
            string("\"Two\\nlines\"").unwrap(),
            ("", "Two\\nlines".to_owned())
        );
        assert_eq!(
            string("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"").unwrap(),
            ("", "Ring\\^GMy\\7Bell\\007!".to_owned())
        );

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            lit("\"Hello, World!\"").unwrap(),
            ("", Lit::String("Hello, World!".to_owned()))
        );
        assert_eq!(
            lit("\"Two\\nlines\"").unwrap(),
            ("", Lit::String("Two\\nlines".to_owned()))
        );
        assert_eq!(
            lit("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"").unwrap(),
            ("", Lit::String("Ring\\^GMy\\7Bell\\007!".to_owned()))
        );

        // Mindless sanity check
        assert_ne!(string("\"Foo\"").unwrap(), ("", "Bar".to_owned()));

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_variables() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(var("X").unwrap(), ("", Var("X".to_owned())));
        assert_eq!(var("Bar").unwrap(), ("", Var("Bar".to_owned())));
        assert_eq!(var("Value_2").unwrap(), ("", Var("Value_2".to_owned())));
        assert_eq!(var("One2Three").unwrap(), ("", Var("One2Three".to_owned())));
        assert_eq!(var("Stay@home").unwrap(), ("", Var("Stay@home".to_owned())));
        assert_eq!(
            var("_hello_world").unwrap(),
            ("", Var("_hello_world".to_owned()))
        );

        // Lowercase var must give error
        assert!(var("lowercase").is_err());

        // Core erlang accepts "_" as a var despite spec version 1.03 explicitly says this is invalid
        assert_eq!(var("_").unwrap(), ("", Var("_".to_owned())));

        // Mindless sanity check
        assert_ne!(var("A").unwrap(), ("", Var("B".to_owned())));

        // TODO: Negative / Expect Error test
    }
}
