use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::alpha1,
    combinator::{map, value},
    multi::separated_list1,
    sequence::{delimited, pair, tuple},
    IResult,
};

use crate::cerl_parser::{
    ast::{Fname, FunHead, Integer},
    helpers::ws,
    terminals::atom,
};

use super::ast::{ElmType, Session, SessionMode, SessionType};

use crate::st_parser::parser::SessionMode::{Fresh, NotST, Ongoing};

// TODO: Reuse of functions from cerl parser allows for comments and maybe annotations inside the ST which is odd
// Better to not reuse or rewrite so the core can be reused rather than the whole code.
// Doing for now to get a working prototype
pub fn st_parse(i: &str) -> IResult<&str, Session> {
    map(
        tuple((
            atom,
            delimited(
                tag("("),
                separated_list1(
                    tag(","),
                    alt((fresh_st, ongoing_st, value(NotST, tag("_")))),
                ),
                tag(")"),
            ),
        )),
        |(fname, sm)| Session {
            name: FunHead {
                name: Fname(fname),
                arity: Integer(sm.len().try_into().unwrap()),
            },
            st: sm,
        },
    )(i)
}

fn fresh_st(i: &str) -> IResult<&str, SessionMode> {
    map(
        tuple((ws(tag("fresh")), delimited(tag("("), st_inner, tag(")")))),
        |(_, o)| Fresh(o),
    )(i)
}

fn ongoing_st(i: &str) -> IResult<&str, SessionMode> {
    map(
        tuple((
            ws(tag("ongoing")),
            delimited(tag("("), pair(st_inner, st_inner), tag(")")),
        )),
        |(_, (o2, o3))| Ongoing(o2, o3),
    )(i)
}

fn st_inner(i: &str) -> IResult<&str, Vec<SessionType>> {
    map(
        pair(
            separated_list1(ws(tag(".")), alt((st_send, st_receive))), // TODO: Add branch and choice
            tag("."),
        ),
        |(o, _)| o,
    )(i)
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_send(i: &str) -> IResult<&str, SessionType> {
    let (i, _) = tag("!")(i)?;
    let (i, o) = alpha1(i)?;
    Ok((i, SessionType::Send(ElmType(o.to_string()))))
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_receive(i: &str) -> IResult<&str, SessionType> {
    let (i, _) = tag("?")(i)?;
    let (i, o) = alpha1(i)?;
    Ok((i, SessionType::Receive(ElmType(o.to_string()))))
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::ast::{Atom, Fname, FunHead, Integer};
    use crate::st_parser::ast::ElmType;
    use crate::st_parser::ast::SessionMode::{Fresh, NotST};
    use crate::st_parser::ast::SessionType;

    use super::*;

    #[test]
    fn simple_session() {
        assert_eq!(
            st_parse("'test'(_,fresh(!int.))"),
            Ok((
                "",
                Session {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Fresh(vec!(SessionType::Send(ElmType("int".to_owned()))))
                    )
                }
            ))
        );
    }
}
