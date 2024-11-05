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
    cinput::CInput,
    helpers::{loc, wsa, wsc},
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

fn escapechar(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
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

fn octal(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    map(oct_digit1, |o: CInput| {
        u8::from_str_radix(o.input, 8).unwrap() as char
    })(i)
}

fn escape(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    preceded(
        char('\\'),
        alt((octal, preceded(char('^'), ctrlchar), escapechar)),
    )(i)
}

fn ctrlchar(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    map(verify(anychar, |o| is_ctlchar(*o as u8)), |o| {
        (o as u8 - 64) as char
    })(i)
}

// TODO: Accept atoms with escaped chars
// Inspired by :
// - https://docs.rs/nom/latest/nom/recipes/index.html#escaped-strings
// - https://github.com/rust-bakery/nom/blob/main/examples/string.rs
// WSA OK
pub fn atom(i: CInput) -> IResult<CInput, Rc<Atom>, ErrorTree<CInput>> {
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
pub fn fname(i: CInput) -> IResult<CInput, Rc<FunName>, ErrorTree<CInput>> {
    map(
        tuple((
            atom,
            wsa(tag("/")),
            map_res(wsa(digit1), |o: CInput| str::parse::<usize>(o.input)),
        )),
        |(name, _, arity)| (FunName { name, arity }).into(),
    )(i)
}

// WSA OK
pub fn integer<
    'a,
    E: ParseError<CInput<'a>>
        + nom::error::FromExternalError<CInput<'a>, std::num::ParseIntError>
        + nom::error::ParseError<CInput<'a>>,
>(
    i: CInput<'a>,
) -> IResult<CInput, i64, E> {
    let (i, res) = map(
        tuple((
            // Consume + or - or nothing and save the sign
            alt((
                value(true, char('+')),
                (value(false, char('-'))),
                success(true),
            )),
            // Read integer value
            map_res(digit1, |o: CInput| str::parse::<i64>(o.input)),
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
    E: ParseError<CInput<'a>> + nom::error::FromExternalError<CInput<'a>, std::num::ParseIntError>,
>(
    i: CInput<'a>,
) -> IResult<CInput, i64, E> {
    let (i, sign) = opt(alt((char('+'), char('-'))))(i)?;
    match sign {
        None => map_res(digit1, |o: CInput| str::parse::<i64>(o.input))(i),
        Some(sign) => match sign {
            '+' => map_res(digit1, |o: CInput| str::parse::<i64>(o.input))(i),
            '-' => {let (i, res) = map_res(digit1, |o:CInput| str::parse::<i64>(o.input))(i)?; Ok((i,-res))},
            // TODO: Can I get rid of the panic here ?
            _ => panic!("Sign not + or - should never be reachable after checking the value is either of those two.")
        }
    }
}

// The build in float function in nom is not enough.
// WSA OK

pub fn float(i: CInput) -> IResult<CInput, Rc<Float>, ErrorTree<CInput>> {
    map(loc(float_aux), |(loc, (base, decimal, exponent))| {
        Float {
            loc,
            base,
            decimal,
            exponent,
        }
        .into()
    })(i)
}

fn float_aux<
    'a,
    E: ParseError<CInput<'a>> + nom::error::FromExternalError<CInput<'a>, std::num::ParseIntError>,
>(
    i: CInput<'a>,
) -> IResult<CInput, (i64, u64, i64), E> {
    // TODO: usize and isize instead of u64 and i64
    let (i, (base, _, decimal, exponent)) = tuple((
        opt_sign_digit1,
        char('.'),
        map_res(digit1, |o: CInput| str::parse::<u64>(o.input)),
        opt(tuple((alt((char('E'), char('e'))), opt_sign_digit1))),
    ))(i)?;
    let exponent = match exponent {
        Some((_, res)) => res,
        None => 1,
    };
    let (i, _) = wsc(i).unwrap(); // Safe since whitespace can never fail.
    Ok((i, (base, decimal, exponent)))
}

// WSA OK
pub fn string(i: CInput) -> IResult<CInput, String, ErrorTree<CInput>> {
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

fn quoted_string(i: CInput) -> IResult<CInput, String, ErrorTree<CInput>> {
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
pub fn char_char(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
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

fn atom_char(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    alt((
        verify(inputchar, |o| {
            !is_control(*o as u8) && *o != '\\' && *o != '\''
        }),
        escape,
    ))(i)
}

fn string_char(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    alt((
        verify(inputchar, |o| {
            !is_control(*o as u8) && *o != '\\' && *o != '"'
        }),
        escape,
    ))(i)
}

fn inputchar(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    verify(anychar, |o| is_inputchar(*o as u8))(i)
}

// TODO: There must exist a easier way to do this
fn namechar(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    verify(anychar, |o| is_namechar(*o as u8))(i)
}

fn uppercase(i: CInput) -> IResult<CInput, char, ErrorTree<CInput>> {
    verify(anychar, |o| is_uppercase(*o as u8))(i)
}

// WSA OK
pub fn var(i: CInput) -> IResult<CInput, Rc<Var>, ErrorTree<CInput>> {
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
    use crate::cerl_parser::ast::{CLoc, Loc};
    use crate::cerl_parser::helpers::cts;
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
        assert_eq!(cts(integer::<()>(CInput::new("8")).unwrap()), ("", 8));
        assert_eq!(cts(integer::<()>(CInput::new("+17")).unwrap()), ("", 17));
        assert_eq!(
            cts(integer::<()>(CInput::new("299792458")).unwrap()),
            ("", 299792458)
        );
        assert_eq!(
            cts(integer::<()>(CInput::new("-4711")).unwrap()),
            ("", -4711)
        );

        // TODO: Somehow make "::<()>" optional instead of having to type it...
        assert_eq!(cts(literal(CInput::new("8")).unwrap()), ("", Lit::Int(8)));
        assert_eq!(
            cts(literal(CInput::new("+17")).unwrap()),
            ("", Lit::Int(17))
        );
        assert_eq!(
            cts(literal(CInput::new("299792458")).unwrap()),
            ("", Lit::Int(299792458))
        );
        assert_eq!(
            cts(literal(CInput::new("-4711")).unwrap()),
            ("", Lit::Int(-4711))
        );

        // TODO: Negative / Expect Error test
    }

    // TODO: Could not get direct test on float to work, using lit from lex.rs
    #[test]
    fn test_floating_point_numbers() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            cts(float(CInput::new("0.0")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 3 },
                    }
                    .into(),
                    base: 0,
                    decimal: 0,
                    exponent: 1
                }
                .into()
            )
        );
        assert_eq!(
            cts(float(CInput::new("2.7182818")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 9 },
                    }
                    .into(),
                    base: 2,
                    decimal: 7182818,
                    exponent: 1
                }
                .into()
            )
        );
        assert_eq!(
            cts(float(CInput::new("-3.14")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 5 },
                    }
                    .into(),
                    base: -3,
                    decimal: 14,
                    exponent: 1
                }
                .into()
            )
        );
        assert_eq!(
            cts(float(CInput::new("+1.2E-6")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 7 },
                    }
                    .into(),
                    base: 1,
                    decimal: 2,
                    exponent: -6
                }
                .into()
            )
        );
        assert_eq!(
            cts(float(CInput::new("-1.23e12")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 8 },
                    }
                    .into(),
                    base: -1,
                    decimal: 23,
                    exponent: 12
                }
                .into()
            )
        );
        assert_eq!(
            cts(float(CInput::new("1.0e+9")).unwrap()),
            (
                "",
                Float {
                    loc: CLoc {
                        comment: None,
                        start: Loc { line: 1, column: 1 },
                        end: Loc { line: 1, column: 6 },
                    }
                    .into(),
                    base: 1,
                    decimal: 0,
                    exponent: 9
                }
                .into()
            )
        );

        assert_eq!(
            cts(literal(CInput::new("0.0")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 3 },
                        }
                        .into(),
                        base: 0,
                        decimal: 0,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            cts(literal(CInput::new("2.7182818")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 9 },
                        }
                        .into(),
                        base: 2,
                        decimal: 7182818,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            cts(literal(CInput::new("-3.14")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 5 },
                        }
                        .into(),
                        base: -3,
                        decimal: 14,
                        exponent: 1
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            cts(literal(CInput::new("+1.2E-6")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 7 },
                        }
                        .into(),
                        base: 1,
                        decimal: 2,
                        exponent: -6
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            cts(literal(CInput::new("-1.23e12")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 8 },
                        }
                        .into(),
                        base: -1,
                        decimal: 23,
                        exponent: 12
                    }
                    .into()
                )
            )
        );
        assert_eq!(
            cts(literal(CInput::new("1.0e+9")).unwrap()),
            (
                "",
                Lit::Float(
                    Float {
                        loc: CLoc {
                            comment: None,
                            start: Loc { line: 1, column: 1 },
                            end: Loc { line: 1, column: 6 },
                        }
                        .into(),
                        base: 1,
                        decimal: 0,
                        exponent: 9
                    }
                    .into()
                )
            )
        );

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_atom() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            cts(atom(CInput::new("'foo'")).unwrap()),
            ("", Atom("foo".to_owned()).into())
        );
        assert_eq!(
            cts(atom(CInput::new("'Bar'")).unwrap()),
            ("", Atom("Bar".to_owned()).into())
        );
        assert_eq!(
            cts(atom(CInput::new("'foo bar'")).unwrap()),
            ("", Atom("foo bar".to_owned()).into())
        );
        assert_eq!(
            cts(atom(CInput::new("''")).unwrap()),
            ("", Atom("".to_owned()).into())
        );

        // TODO: Is the test correct with \\ == \ in the string?
        assert_eq!(cts(octal(CInput::new("012")).unwrap()), ("", 10 as char));
        assert_eq!(
            cts(atom(CInput::new("'\\010'")).unwrap()),
            ("", Atom("\u{8}".to_owned()).into())
        );

        assert_eq!(
            cts(atom(CInput::new("'_hello_world'")).unwrap()),
            ("", Atom("_hello_world".to_owned()).into())
        );
        assert_eq!(
            cts(atom(CInput::new("'=:='")).unwrap()),
            ("", Atom("=:=".to_owned()).into())
        );

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            cts(literal(CInput::new("'foo'")).unwrap()),
            ("", Lit::Atom(Atom("foo".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("'Bar'")).unwrap()),
            ("", Lit::Atom(Atom("Bar".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("'foo bar'")).unwrap()),
            ("", Lit::Atom(Atom("foo bar".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("''")).unwrap()),
            ("", Lit::Atom(Atom("".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("'%#\\010@\\n!'")).unwrap()),
            ("", Lit::Atom(Atom("%#\u{8}@\n!".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("'_hello_world'")).unwrap()),
            ("", Lit::Atom(Atom("_hello_world".to_owned()).into()))
        );
        assert_eq!(
            cts(literal(CInput::new("'=:='")).unwrap()),
            ("", Lit::Atom(Atom("=:=".to_owned()).into()))
        );
    }

    #[test]
    fn test_char_literal() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(cts(char_char(CInput::new("$A")).unwrap()), ("", 'A'));
        assert_eq!(cts(char_char(CInput::new("$$")).unwrap()), ("", '$'));
        assert_eq!(cts(char_char(CInput::new("$\\n")).unwrap()), ("", '\n'));
        assert_eq!(cts(char_char(CInput::new("$\\s")).unwrap()), ("", ' '));
        assert_eq!(cts(char_char(CInput::new("$\\\\")).unwrap()), ("", '\\'));
        assert_eq!(cts(char_char(CInput::new("$\\12")).unwrap()), ("", '\u{A}'));
        assert_eq!(cts(char_char(CInput::new("$\\101")).unwrap()), ("", 'A'));
        assert_eq!(
            cts(char_char(CInput::new("$\\^A")).unwrap()),
            ("", '\u{0001}')
        );

        // TODO Move "lit" tests to lex.rs ?
        assert_eq!(
            cts(literal(CInput::new("$A")).unwrap()),
            ("", Lit::Char('A'))
        );
        assert_eq!(
            cts(literal(CInput::new("$$")).unwrap()),
            ("", Lit::Char('$'))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\n")).unwrap()),
            ("", Lit::Char('\n'))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\s")).unwrap()),
            ("", Lit::Char(' '))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\\\")).unwrap()),
            ("", Lit::Char('\\'))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\12")).unwrap()),
            ("", Lit::Char('\u{A}'))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\101")).unwrap()),
            ("", Lit::Char('A'))
        );
        assert_eq!(
            cts(literal(CInput::new("$\\^A")).unwrap()),
            ("", Lit::Char('\u{0001}'))
        );

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_strings() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            cts(string(CInput::new("\"Hello, World!\"")).unwrap()),
            ("", "Hello, World!".to_owned())
        );
        assert_eq!(
            cts(string(CInput::new("\"Two\\nlines\"")).unwrap()),
            ("", "Two\nlines".to_owned())
        );
        assert_eq!(
            cts(string(CInput::new("\"\"")).unwrap()),
            ("", "".to_owned())
        );
        assert_eq!(
            cts(string(CInput::new("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"")).unwrap()),
            ("", "Ring\u{7}My\u{7}Bell\u{7}!".to_owned())
        );

        assert_eq!(
            cts(literal(CInput::new("\"Hello, World!\"")).unwrap()),
            ("", Lit::String("Hello, World!".to_owned()))
        );
        assert_eq!(
            cts(literal(CInput::new("\"Two\\nlines\"")).unwrap()),
            ("", Lit::String("Two\nlines".to_owned()))
        );
        assert_eq!(
            cts(literal(CInput::new("\"\"")).unwrap()),
            ("", Lit::String("".to_owned()))
        );
        assert_eq!(
            cts(literal(CInput::new("\"Ring\\^G\" \"My\\7\" \"Bell\\007!\"")).unwrap()),
            ("", Lit::String("Ring\u{7}My\u{7}Bell\u{7}!".to_owned()))
        );

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_variables() {
        // Tests based on Core Erlang 1.03 specification Appendix A
        assert_eq!(
            cts(var(CInput::new("X")).unwrap()),
            ("", Var("X".to_owned()).into())
        );
        assert_eq!(
            cts(var(CInput::new("Bar")).unwrap()),
            ("", Var("Bar".to_owned()).into())
        );
        assert_eq!(
            cts(var(CInput::new("Value_2")).unwrap()),
            ("", Var("Value_2".to_owned()).into())
        );
        assert_eq!(
            cts(var(CInput::new("One2Three")).unwrap()),
            ("", Var("One2Three".to_owned()).into())
        );
        assert_eq!(
            cts(var(CInput::new("Stay@home")).unwrap()),
            ("", Var("Stay@home".to_owned()).into())
        );
        assert_eq!(
            cts(var(CInput::new("_hello_world")).unwrap()),
            ("", Var("_hello_world".to_owned()).into())
        );

        // Lowercase var must give error
        assert!(var(CInput::new("lowercase")).is_err());

        // Core erlang accepts "_" as a var despite spec version 1.03 explicitly says this is invalid
        assert_eq!(
            cts(var(CInput::new("_")).unwrap()),
            ("", Var("_".to_owned()).into())
        );

        // TODO: Negative / Expect Error test
    }

    #[test]
    fn test_lit_atom_in_list() {
        assert_eq!(
            cts(literal(CInput::new("'new', 'neg')")).unwrap()),
            (", 'neg')", Lit::Atom(Atom("new".to_owned()).into()))
        );
    }
}
