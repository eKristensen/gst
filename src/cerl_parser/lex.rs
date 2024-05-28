// TODO: Does a "Lexer" file even make sense with Nom ??

use nom::{
    branch::alt,
    character::{complete::digit1, is_digit},
    combinator::{map, map_res},
    sequence::tuple,
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{FunName, Lit},
    helpers::{comma_sep_list, opt_annotation, ws},
    terminals::{atom, char_char, float, integer, string},
};

#[inline]
pub fn is_lowercase(chr: u8) -> bool {
    (0x61..=0x7A).contains(&chr) || (0xC0..=0xD6).contains(&chr) || (0xD8..=0xDE).contains(&chr)
}

#[inline]
pub fn is_uppercase(chr: u8) -> bool {
    (0x41..=0x5A).contains(&chr) || (0xDF..=0xF6).contains(&chr) || (chr >= 0xF8)
}

#[inline]
pub fn is_inputchar(chr: u8) -> bool {
    chr != 0x0D && chr != 0x0A
}

#[inline]
pub fn is_control(chr: u8) -> bool {
    chr <= 0x1F
}

#[inline]
pub fn is_namechar(chr: u8) -> bool {
    is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == '@' as u8 || chr == '_' as u8
}

// Built into nom: is_oct_digit(chr)

pub fn is_ctlchar(chr: u8) -> bool {
    (0x40..=0x5F).contains(&chr)
}

// TODO: Move to terminals?
pub fn fname(i: &str) -> IResult<&str, FunName, ErrorTree<&str>> {
    opt_annotation(fname_inner)(i)
}

// TODO: Move to terminals?
pub fn fname_inner(i: &str) -> IResult<&str, FunName, ErrorTree<&str>> {
    ws(map(
        tuple((
            atom,
            tag("/"), // TODO: Check whether whitespace is allowed around this tag in this context
            map_res(digit1, str::parse::<u64>),
        )),
        |(name, _, arity)| FunName { name, arity },
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
        map(char_char, crate::cerl_parser::ast::Lit::Char),
        map(string, crate::cerl_parser::ast::Lit::String),
        lit_nested_list,
        map(comma_sep_list("[", "]", lit), super::ast::Lit::Cons),
        map(
            comma_sep_list("{", "}", lit),
            crate::cerl_parser::ast::Lit::Tuple,
        ),
    ))(i)
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
