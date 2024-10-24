use std::rc::Rc;

use nom::{
    branch::alt,
    character::{
        complete::{anychar, char, digit1, oct_digit1},
        is_digit,
    },
    combinator::{map, map_res, opt, success, value, verify},
    error::ParseError,
    multi::{fold_many0, fold_many1},
    sequence::{delimited, pair, preceded, tuple},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Atom, Float, FunName, Var},
    helpers::{wsa, wsc},
};

#[inline]
fn is_lowercase(chr: u8) -> bool {
    (0x61..=0x7A).contains(&chr) || (0xC0..=0xD6).contains(&chr) || (0xD8..=0xDE).contains(&chr)
}

#[inline]
fn is_uppercase(chr: u8) -> bool {
    (0x41..=0x5A).contains(&chr) || (0xDF..=0xF6).contains(&chr) || (chr >= 0xF8)
}

#[inline]
fn is_inputchar(chr: u8) -> bool {
    chr != 0x0D && chr != 0x0A
}

#[inline]
fn is_control(chr: u8) -> bool {
    chr <= 0x1F
}

#[inline]
fn is_namechar(chr: u8) -> bool {
    is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == b'@' || chr == b'_'
}

#[inline]
fn is_ctlchar(chr: u8) -> bool {
    (0x40..=0x5F).contains(&chr)
}
// Built into nom: is_oct_digit(chr)

fn escapechar(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    alt((
        value(0x0008 as char, char('b')), // escapechar
        value(0x007F as char, char('d')),
        value(0x001B as char, char('e')),
        value(0x000C as char, char('f')),
        value(0x000A as char, char('n')),
        value(0x000D as char, char('r')),
        value(0x0020 as char, char('s')),
        value(0x0009 as char, char('t')),
        value(0x000B as char, char('v')),
        value(0x0022 as char, char('"')),
        value(0x0027 as char, char('\'')),
        value(0x005C as char, char('\\')),
    ))(i)
}

fn octal(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    map(oct_digit1, |o| u8::from_str_radix(o, 8).unwrap() as char)(i)
}

fn escape(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    preceded(
        char('\\'),
        alt((octal, preceded(char('^'), ctrlchar), escapechar)),
    )(i)
}

fn ctrlchar(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    map(verify(anychar, |o| is_ctlchar(*o as u8)), |o| {
        (o as u8 - 64) as char
    })(i)
}

// TODO: Accept atoms with escaped chars
// Inspired by :
// - https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
// - https://github.com/rust-bakery/nom/blob/main/examples/string.rs
// WSA OK
pub fn atom(i: &str) -> IResult<&str, Rc<Atom>, ErrorTree<&str>> {
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        atom_char,
        // Our init value, an empty string
        Vec::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            string.push(fragment);
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    wsa(map(delimited(char('\''), build_string, char('\'')), |o| {
        Atom(o.iter().collect()).into()
    }))(i)
}

// WSA OK
pub fn fname(i: &str) -> IResult<&str, Rc<FunName>, ErrorTree<&str>> {
    map(
        tuple((
            atom,
            wsa(tag("/")),
            map_res(wsa(digit1), str::parse::<usize>),
        )),
        |(name, _, arity)| (FunName { name, arity }).into(),
    )(i)
}

// WSA OK
pub fn integer<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, i64, E> {
    let (i, res) = map(
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
    )(i)?;
    let (i, _) = wsc(i).unwrap(); // Safe because whitespace can never fail.
    Ok((i, res))
}

pub fn opt_sign_digit1<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, i64, E> {
    let (i, sign) = opt(alt((char('+'), char('-'))))(i)?;
    match sign {
        None => map_res(digit1, str::parse::<i64>)(i),
        Some(sign) => match sign {
            '+' => map_res(digit1, str::parse::<i64>)(i),
            '-' => {let (i, res) = map_res(digit1, str::parse::<i64>)(i)?; Ok((i,-res))},
            // TODO: Can I get rid of the panic here ?
            _ => panic!("Sign not + or - should never be reachable after checking the value is either of those two.")
        }
    }
}

// The build in float function in nom is not enough.
// WSA OK
pub fn float<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Rc<Float>, E> {
    let (i, (base, _, decimal, exponent)) = tuple((
        opt_sign_digit1,
        char('.'),
        map_res(digit1, str::parse::<u64>),
        opt(tuple((alt((char('E'), char('e'))), opt_sign_digit1))),
    ))(i)?;
    let exponent = match exponent {
        Some((_, res)) => res,
        None => 1,
    };
    let (i, _) = wsc(i).unwrap(); // Safe since whitespace can never fail.
    Ok((
        i,
        Float {
            base,
            decimal,
            exponent,
        }
        .into(),
    ))
}

// WSA OK
pub fn string(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    // From the cerl spec versin 1.0.3 section 3:
    // > The tokenisation should concatenate all adjacent string literals (these may
    // > be separated by any numb er of whitespace characters, line terminators and
    // > comments). Thus, the text \"Hey" "Ho"" denotes the same string literal as
    // > "HeyHo". This allows strings to be split over several lines.
    fold_many1(wsa(quoted_string), String::new, |mut string, fragment| {
        string.push_str(&fragment);
        string
    })(i)
}

fn quoted_string(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        string_char,
        // Our init value, an empty string
        Vec::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut fold_var, fragment| {
            fold_var.push(fragment);
            fold_var
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    map(delimited(char('"'), build_string, char('"')), |o| {
        o.iter().collect()
    })
    .parse(i)
}

// WSA OK
pub fn char_char(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    wsa(preceded(
        char('$'),
        alt((
            verify(inputchar, |o| {
                !is_control(*o as u8) && *o != ' ' && *o != '\\'
            }),
            escape,
        )),
    ))(i)
}

fn atom_char(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    alt((
        verify(inputchar, |o| {
            !is_control(*o as u8) && *o != '\\' && *o != '\''
        }),
        escape,
    ))(i)
}

fn string_char(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    alt((
        verify(inputchar, |o| {
            !is_control(*o as u8) && *o != '\\' && *o != '"'
        }),
        escape,
    ))(i)
}

fn inputchar(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    verify(anychar, |o| is_inputchar(*o as u8))(i)
}

// TODO: There must exist a easier way to do this
fn namechar(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    verify(anychar, |o| is_namechar(*o as u8))(i)
}

fn uppercase(i: &str) -> IResult<&str, char, ErrorTree<&str>> {
    verify(anychar, |o| is_uppercase(*o as u8))(i)
}

// WSA OK
pub fn var(i: &str) -> IResult<&str, Rc<Var>, ErrorTree<&str>> {
    wsa(map(
        pair(
            alt((
                uppercase,
                char('_'), // Note: Odd but elc accepts "_" as a valid variable name despite it explicitly being invalid in the core erlang specification
            )),
            fold_many0(namechar, Vec::new, |mut string, fragment| {
                string.push(fragment);
                string
            }),
        ),
        |(o1, o2)| {
            let mut var_name = vec![o1];
            var_name.extend(o2);
            Rc::new(Var(var_name.iter().collect()))
        },
    ))(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::{ast::Lit, expressions::literal};

    use super::*;

    #[test]
    fn test_is_ctrlchar() {
        // Sanity checks
        assert!(!is_ctlchar(0x3F));
        assert!(is_ctlchar(0x40));
        assert!(is_ctlchar(0x50));
        assert!(is_ctlchar(0x5F));
        assert!(!is_ctlchar(0x60))
    }

    #[test]
    fn test_integer_literals() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(integer::<()>("8"), Ok(("", 8)));
        assert_eq!(integer::<()>("+17"), Ok(("", 17)));
        assert_eq!(integer::<()>("299792458"), Ok(("", 299792458)));
        assert_eq!(integer::<()>("-4711"), Ok(("", -4711)));

        // TODO Move "lit" tests to lex.rs ?
        // TODO: Somehow make "::<()>" optional instead of having to type it...
        assert_eq!(literal("8").unwrap(), ("", Lit::Int(8)));
        assert_eq!(literal("+17").unwrap(), ("", Lit::Int(17)));
        assert_eq!(literal("299792458").unwrap(), ("", Lit::Int(299792458)));
        assert_eq!(literal("-4711").unwrap(), ("", Lit::Int(-4711)));

        // Mindless sanity check
        assert_ne!(literal("8").unwrap(), ("", Lit::Int(42)));

        // TODO: Negative / Expect Error test
    }

    // TODO: Could not get direct test on float to work, using lit from lex.rs
    #[test]
    fn test_floating_point_numbers() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            float::<()>("0.0"),
            Ok((
                "",
                Float {
                    base: 0,
                    decimal: 0,
                    exponent: 1
                }
                .into()
            ))
        );
        assert_eq!(
            float::<()>("2.7182818"),
            Ok((
                "",
                Float {
                    base: 2,
                    decimal: 7182818,
                    exponent: 1
                }
                .into()
            ))
        );
        assert_eq!(
            float::<()>("-3.14"),
            Ok((
                "",
                Float {
                    base: -3,
                    decimal: 14,
                    exponent: 1
                }
                .into()
            ))
        );
        assert_eq!(
            float::<()>("+1.2E-6"),
            Ok((
                "",
                Float {
                    base: 1,
                    decimal: 2,
                    exponent: -6
                }
                .into()
            ))
        );
        assert_eq!(
            float::<()>("-1.23e12"),
            Ok((
                "",
                Float {
                    base: -1,
                    decimal: 23,
                    exponent: 12
                }
                .into()
            ))
        );
        assert_eq!(
            float::<()>("1.0e+9"),
            Ok((
                "",
                Float {
                    base: 1,
                    decimal: 0,
                    exponent: 9
                }
                .into()
            ))
        );

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            literal("0.0").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: 0,
                        decimal: 0,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            literal("2.7182818").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: 2,
                        decimal: 7182818,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            literal("-3.14").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: -3,
                        decimal: 14,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            literal("+1.2E-6").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: 1,
                        decimal: 2,
                        exponent: -6
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            literal("-1.23e12").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: -1,
                        decimal: 23,
                        exponent: 12
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            literal("1.0e+9").unwrap(),
            (
                "",
                Lit::Float(
                    Float {
                        base: 1,
                        decimal: 0,
                        exponent: 9
                    }
                    .into()
                )
            )
        );

        // Mindless sanity check
        assert_ne!(
            float::<()>("0.0"),
            Ok((
                "",
                Float {
                    base: 2,
                    decimal: 0,
                    exponent: 1
                }
                .into()
            ))
        );

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_atom() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(atom("'foo'").unwrap(), ("", Atom("foo".to_owned()).into()));
        assert_eq!(atom("'Bar'").unwrap(), ("", Atom("Bar".to_owned()).into()));
        assert_eq!(
            atom("'foo bar'").unwrap(),
            ("", Atom("foo bar".to_owned()).into())
        );
        assert_eq!(atom("''").unwrap(), ("", Atom("".to_owned()).into()));

        // TODO: Is the test correct with \\ == \ in the string?
        assert_eq!(octal("012").unwrap(), ("", 10 as char));
        assert_eq!(
            atom("'\\010'").unwrap(),
            ("", Atom("\u{8}".to_owned()).into())
        );

        assert_eq!(
            atom("'_hello_world'").unwrap(),
            ("", Atom("_hello_world".to_owned()).into())
        );
        assert_eq!(atom("'=:='").unwrap(), ("", Atom("=:=".to_owned()).into()));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            literal("'foo'").unwrap(),
            ("", Lit::Atom(Atom("foo".to_owned()).into()))
        );
        assert_eq!(
            literal("'Bar'").unwrap(),
            ("", Lit::Atom(Atom("Bar".to_owned()).into()))
        );
        assert_eq!(
            literal("'foo bar'").unwrap(),
            ("", Lit::Atom(Atom("foo bar".to_owned()).into()))
        );
        assert_eq!(
            literal("''").unwrap(),
            ("", Lit::Atom(Atom("".to_owned()).into()))
        );
        assert_eq!(
            literal("'%#\\010@\\n!'").unwrap(),
            ("", Lit::Atom(Atom("%#\u{8}@\n!".to_owned()).into()))
        );
        assert_eq!(
            literal("'_hello_world'").unwrap(),
            ("", Lit::Atom(Atom("_hello_world".to_owned()).into()))
        );
        assert_eq!(
            literal("'=:='").unwrap(),
            ("", Lit::Atom(Atom("=:=".to_owned()).into()))
        );

        // Mindless sanity check
        assert_ne!(atom("'foo'").unwrap(), ("", Atom("bar".to_owned()).into()));
    }

    #[test]
    fn test_char_literal() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(char_char("$A").unwrap(), ("", 'A'));
        assert_eq!(char_char("$$").unwrap(), ("", '$'));
        assert_eq!(char_char("$\\n").unwrap(), ("", '\n'));
        assert_eq!(char_char("$\\s").unwrap(), ("", ' '));
        assert_eq!(char_char("$\\\\").unwrap(), ("", '\\'));
        assert_eq!(char_char("$\\12").unwrap(), ("", '\u{A}'));
        assert_eq!(char_char("$\\101").unwrap(), ("", 'A'));
        assert_eq!(char_char("$\\^A").unwrap(), ("", '\u{0001}'));

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(literal("$A").unwrap(), ("", Lit::Char('A')));
        assert_eq!(literal("$$").unwrap(), ("", Lit::Char('$')));
        assert_eq!(literal("$\\n").unwrap(), ("", Lit::Char('\n')));
        assert_eq!(literal("$\\s").unwrap(), ("", Lit::Char(' ')));
        assert_eq!(literal("$\\\\").unwrap(), ("", Lit::Char('\\')));
        assert_eq!(literal("$\\12").unwrap(), ("", Lit::Char('\u{A}')));
        assert_eq!(literal("$\\101").unwrap(), ("", Lit::Char('A')));
        assert_eq!(literal("$\\^A").unwrap(), ("", Lit::Char('\u{0001}')));

        // Mindless sanity check
        assert_ne!(char_char("$A").unwrap(), ("", 'B'));

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
            ("", "Two\nlines".to_owned())
        );
        assert_eq!(string("\"\"").unwrap(), ("", "".to_owned()));
        assert_eq!(
            string("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"").unwrap(),
            ("", "Ring\u{7}My\u{7}Bell\u{7}!".to_owned())
        );

        assert_eq!(
            literal("\"Hello, World!\"").unwrap(),
            ("", Lit::String("Hello, World!".to_owned()))
        );
        assert_eq!(
            literal("\"Two\\nlines\"").unwrap(),
            ("", Lit::String("Two\nlines".to_owned()))
        );
        assert_eq!(literal("\"\"").unwrap(), ("", Lit::String("".to_owned())));
        assert_eq!(
            literal("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"").unwrap(),
            ("", Lit::String("Ring\u{7}My\u{7}Bell\u{7}!".to_owned()))
        );

        // Mindless sanity check
        assert_ne!(string("\"Foo\"").unwrap(), ("", "Bar".to_owned()));

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_variables() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(var("X").unwrap(), ("", Var("X".to_owned()).into()));
        assert_eq!(var("Bar").unwrap(), ("", Var("Bar".to_owned()).into()));
        assert_eq!(
            var("Value_2").unwrap(),
            ("", Var("Value_2".to_owned()).into())
        );
        assert_eq!(
            var("One2Three").unwrap(),
            ("", Var("One2Three".to_owned()).into())
        );
        assert_eq!(
            var("Stay@home").unwrap(),
            ("", Var("Stay@home".to_owned()).into())
        );
        assert_eq!(
            var("_hello_world").unwrap(),
            ("", Var("_hello_world".to_owned()).into())
        );

        // Lowercase var must give error
        assert!(var("lowercase").is_err());

        // Core erlang accepts "_" as a var despite spec version 1.03 explicitly says this is invalid
        assert_eq!(var("_").unwrap(), ("", Var("_".to_owned()).into()));

        // Mindless sanity check
        assert_ne!(var("A").unwrap(), ("", Var("B".to_owned()).into()));

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_lit_atom_in_list() {
        assert_eq!(
            literal("'new', 'neg')").unwrap(),
            (", 'neg')", Lit::Atom(Atom("new".to_owned()).into()))
        );
    }
}
