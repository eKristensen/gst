// TODO: Does a "Lexer" file even make sense with Nom ??

use nom::{
    branch::alt,
    character::{complete::digit1, is_digit},
    combinator::{map, map_res, value},
    sequence::{pair, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{FunName, FunNameInner, Lit, LitInner},
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
    is_uppercase(chr) || is_lowercase(chr) || is_digit(chr) || chr == b'@' || chr == b'_'
}

// Built into nom: is_oct_digit(chr)

pub fn is_ctlchar(chr: u8) -> bool {
    (0x40..=0x5F).contains(&chr)
}

// TODO: Move to terminals?
pub fn fname(i: &str) -> IResult<&str, FunName, ErrorTree<&str>> {
    map(opt_annotation(fname_inner), |(inner, anno)| FunName {
        anno,
        inner,
    })(i)
}

// TODO: Move to terminals?
pub fn fname_inner(i: &str) -> IResult<&str, FunNameInner, ErrorTree<&str>> {
    ws(map(
        tuple((
            atom,
            tag("/"), // TODO: Check whether whitespace is allowed around this tag in this context
            map_res(digit1, str::parse::<usize>),
        )),
        |(name, _, arity)| FunNameInner { name, arity },
    ))(i)
}

// TODO: Common pattern for nested list, avoid manual rewrite!
fn lit_list(i: &str) -> IResult<&str, LitInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, head) = ws(lit)(i)?;
    let (i, _) = ws(tag("|"))(i)?;
    let (i, tail) = ws(lit)(i)?;
    let (i, _) = ws(tag("]"))(i)?;

    Ok((
        i,
        crate::cerl_parser::ast::LitInner::Cons(Box::new((head, tail))),
    ))
}

pub fn lit(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    map(opt_annotation(lit_inner), |(inner, anno)| Lit {
        anno,
        inner,
    })(i)
}

pub fn lit_inner(i: &str) -> IResult<&str, LitInner, ErrorTree<&str>> {
    alt((
        map(float, super::ast::LitInner::Float),
        map(integer, super::ast::LitInner::Int),
        map(atom, |o| crate::cerl_parser::ast::LitInner::Atom(o.name)),
        map(char_char, crate::cerl_parser::ast::LitInner::Char),
        map(string, crate::cerl_parser::ast::LitInner::String),
        lit_list,
        value(super::ast::LitInner::Nil, pair(ws(tag("[")), ws(tag("]")))),
        map(
            comma_sep_list("{", "}", lit),
            crate::cerl_parser::ast::LitInner::Tuple,
        ),
    ))(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::{
        ast::{Anno, Lit, LitInner},
        lex::lit,
    };

    #[test]
    fn test_lit_atom_in_list() {
        assert_eq!(
            lit("'new', 'neg')").unwrap(),
            (
                ", 'neg')",
                Lit {
                    anno: Anno(None),
                    inner: LitInner::Atom("new".to_owned())
                }
            )
        );
    }
}
