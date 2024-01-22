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

use super::ast::{Label, SessionDef, SessionElement, SessionType, Types};

use crate::st_parser::parser::SessionType::{NotST, Server, Session};

// TODO: Reuse of functions from cerl parser allows for comments and maybe annotations inside the ST which is odd
// Better to not reuse or rewrite so the core can be reused rather than the whole code.
// Doing for now to get a working prototype
pub fn st_parse(i: &str) -> IResult<&str, SessionDef> {
    map(
        tuple((
            atom,
            delimited(
                tag("("),
                separated_list1(
                    tag(","),
                    alt((server_st, session_st, value(NotST, tag("_")))),
                ),
                tag(")"),
            ),
        )),
        |(fname, sm)| SessionDef {
            name: FunHead {
                name: Fname(fname),
                arity: Integer(sm.len().try_into().unwrap()),
            },
            st: sm,
        },
    )(i)
}

fn server_st(i: &str) -> IResult<&str, SessionType> {
    map(
        tuple((ws(tag("server")), delimited(tag("("), st_inner, tag(")")))),
        |(_, o)| Server(o),
    )(i)
}

fn session_st(i: &str) -> IResult<&str, SessionType> {
    map(
        tuple((ws(tag("server")), delimited(tag("("), st_inner, tag(")")))),
        |(_, o)| Session(o),
    )(i)
}

fn st_inner(i: &str) -> IResult<&str, Vec<SessionElement>> {
    map(
        pair(
            separated_list1(
                ws(tag(".")),
                alt((
                    st_send,
                    st_receive,
                    st_branch,
                    st_choice,
                    value(SessionElement::End, tag("end")),
                )),
            ), // TODO: Add branch and choice
            tag("."),
        ),
        |(o, _)| o,
    )(i)
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_send(i: &str) -> IResult<&str, SessionElement> {
    let (i, _) = tag("!")(i)?;
    let (i, o) = alpha1(i)?;
    Ok((i, SessionElement::Send(Types::Single(o.to_string()))))
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_receive(i: &str) -> IResult<&str, SessionElement> {
    let (i, _) = tag("?")(i)?;
    let (i, o) = alpha1(i)?;
    Ok((i, SessionElement::Receive(Types::Single(o.to_string()))))
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_branch(i: &str) -> IResult<&str, SessionElement> {
    map(
        delimited(
            tag("&{"),
            separated_list1(tag(","), inner_branch_choice),
            tag("}"),
        ),
        SessionElement::Branch,
    )(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_choice(i: &str) -> IResult<&str, SessionElement> {
    map(
        delimited(
            tag("+{"),
            separated_list1(tag(","), inner_branch_choice),
            tag("}"),
        ),
        SessionElement::Choice,
    )(i)
}

fn inner_branch_choice(i: &str) -> IResult<&str, (Label, SessionType)> {
    map(pair(alpha1, session_st), |(o1, o2)| {
        (Label(o1.to_string()), o2)
    })(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::ast::{Atom, Fname, FunHead, Integer};
    use crate::st_parser::ast::SessionElement;
    use crate::st_parser::ast::SessionType::{NotST, Server};

    use super::*;

    #[test]
    fn simple_session() {
        assert_eq!(
            st_parse("'test'(_,server(!int.))"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Server(vec!(SessionElement::Send(Types::Single("int".to_owned()))))
                    )
                }
            ))
        );
    }

    // TODO: Test _ (NotST), Branch, Choice, Binders
}
