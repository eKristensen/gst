use std::ops::RangeFrom;

use nom::{
    branch::alt,
    character::{complete::digit1, is_digit},
    combinator::{map, map_res},
    error::{ErrorKind, ParseError},
    sequence::tuple,
    AsChar, Err, IResult, InputIter, InputLength, Slice,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{FunName, Lit},
    helpers::{comma_sep_list, opt_annotation, ws},
    terminals::{atom, char_, float, integer, string},
};

#[inline]
fn is_sign(chr: u8) -> bool {
    chr <= 0x2B || chr == 0x2D
}

#[inline]
fn is_lowercase(chr: u8) -> bool {
    (chr >= 0x61 && chr <= 0x7A) || (chr >= 0xC0 && chr <= 0xD6) || (chr >= 0xD8 && chr <= 0xDE)
}

#[inline]
pub fn is_uppercase(chr: u8) -> bool {
    (chr >= 0x41 && chr <= 0x5A) || (chr >= 0xDF && chr <= 0xF6) || (chr >= 0xF8 && chr <= 0xFF)
}

#[inline]
pub fn is_inputchar(chr: u8) -> bool {
    chr != 0x0D && chr != 0x0A
}

#[inline]
pub fn is_control(chr: u8) -> bool {
    chr >= 0x00 && chr <= 0x1F
}

#[inline]
fn is_space(chr: u8) -> bool {
    chr == 0x20
}

#[inline]
pub fn is_namechar(chr: u8) -> bool {
    is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == 0x40 || chr == 0x5F
}

// Built into nom: is_oct_digit(chr)

pub fn is_ctlchar(chr: u8) -> bool {
    chr >= 0x40 && chr <= 0x5F
}

fn is_escapechar(chr: u8) -> bool {
    // bdefnrstv"'\
    chr == 0x62
        || chr == 0x64
        || chr == 0x65
        || chr == 0x66
        || chr == 0x6E
        || chr == 0x72
        || chr == 0x73
        || chr == 0x74
        || chr == 0x76
        || chr == 0x22
        || chr == 0x27
        || chr == 0x5C
}

// TODO: There must exist a easier way to do this
pub fn namechar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
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
    if !(is_namechar(item_chr))
    // With trait it could be item.is_inputchar()
    {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fix)));
    }
    Ok((input, candidate))
}

// Move to "non-terminals"?
pub fn fname(i: &str) -> IResult<&str, FunName, ErrorTree<&str>> {
    opt_annotation(fname_inner)(i)
}

// Move to "non-terminals"?
pub fn fname_inner(i: &str) -> IResult<&str, FunName, ErrorTree<&str>> {
    ws(map(
        tuple((
            atom,
            tag("/"), // TODO: Check whether whitespace is allowed around this tag in this context
            map_res(digit1, str::parse::<u64>),
        )),
        |(name, _, arity)| FunName {
            name: name,
            arity: arity,
        },
    ))(i)
}

// TODO: Common pattern for nested list, avoid manual rewrite!
fn lit_nested_list(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, constant) = ws(lit)(i)?;
    let head = match constant {
        Lit::Cons(inner_list) => inner_list,
        _ => vec![constant],
    };

    let (i, _) = ws(tag("|"))(i)?;
    let (i, constant) = ws(lit)(i)?;
    let tail = match constant {
        Lit::Cons(inner_list) => inner_list,
        _ => vec![constant],
    };
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::cerl_parser::ast::Lit::Cons(cons)))
}

pub fn lit(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    alt((
        map(float, super::ast::Lit::Float),
        map(integer, super::ast::Lit::Int),
        map(atom, crate::cerl_parser::ast::Lit::Atom),
        map(char_, crate::cerl_parser::ast::Lit::Char),
        map(string, crate::cerl_parser::ast::Lit::String),
        lit_nested_list,
        map(comma_sep_list("[", "]", lit), super::ast::Lit::Cons),
        map(
            comma_sep_list("{", "}", lit),
            crate::cerl_parser::ast::Lit::Tuple,
        ),
        empty_list,
    ))(i)
}

fn empty_list(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, _) = ws(tag("]"))(i)?;
    Ok((i, super::ast::Lit::Nil))
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::{
        ast::{Atom, Lit},
        lex::lit,
    };

    #[test]
    fn test_lit_atom_in_list() {
        assert_eq!(
            lit("'new', 'neg')").unwrap(),
            (", 'neg')", Lit::Atom(Atom("new".to_owned())))
        );
    }
}
