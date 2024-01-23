use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::alpha1,
    combinator::{map, value},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::cerl_parser::{
    ast::{Fname, FunHead, Integer, Var},
    helpers::ws,
    terminals::{atom, var},
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
                ws(tag("(")),
                separated_list1(
                    ws(tag(",")),
                    alt((server_st, session_st, value(NotST, ws(tag("_"))))),
                ),
                ws(tag(")")),
            ),
            ws(ws(tag("->"))),
            alt((
                value(vec![], ws(tag("_"))),
                delimited(ws(tag("(")), st_inner, ws(tag(")"))),
            )),
            ws(ws(tag(","))),
            delimited(
                ws(tag("[")),
                separated_list0(
                    ws(tag(",")),
                    pair(var, preceded(ws(ws(tag(":"))), st_inner)),
                ),
                ws(tag("]")),
            ),
        )),
        |(fname, sm, _, rt, _, b)| {
            let mut binders = HashMap::new();

            for (key, elm) in b {
                match binders.insert(key.clone(), elm.clone()) {
                    Some(_) => panic!("Duplicate var in st binders"),
                    None => (),
                }
            }

            SessionDef {
                name: FunHead {
                    name: Fname(fname),
                    arity: Integer(sm.len().try_into().unwrap()),
                },
                st: sm,
                return_type: rt,
                binders,
            }
        },
    )(i)
}

fn server_st(i: &str) -> IResult<&str, SessionType> {
    map(
        tuple((
            ws(tag("server")),
            delimited(ws(tag("(")), st_inner, ws(tag(")"))),
        )),
        |(_, o)| Server(o),
    )(i)
}

fn session_st(i: &str) -> IResult<&str, SessionType> {
    map(
        tuple((
            ws(tag("session")),
            delimited(ws(tag("(")), st_inner, ws(tag(")"))),
        )),
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
                    st_make_choice,
                    st_offer_choice,
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
    map(preceded(tag("!"), alpha1), |o: &str| {
        SessionElement::Send(Types::Single(o.to_string()))
    })(i)
}

fn st_receive(i: &str) -> IResult<&str, SessionElement> {
    map(preceded(tag("?"), alpha1), |o: &str| {
        SessionElement::Receive(Types::Single(o.to_string()))
    })(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_make_choice(i: &str) -> IResult<&str, SessionElement> {
    map(
        delimited(
            ws(tag("&{")),
            separated_list1(ws(tag(",")), inner_choice),
            ws(tag("}")),
        ),
        SessionElement::MakeChoice,
    )(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_offer_choice(i: &str) -> IResult<&str, SessionElement> {
    map(
        delimited(
            ws(tag("+{")),
            separated_list1(ws(tag(",")), inner_choice),
            ws(tag("}")),
        ),
        |o| {
            let mut offer_choice = HashMap::new();

            for (label, elm) in o {
                match offer_choice.insert(label.clone(), elm.clone()) {
                    Some(_) => panic!("Duplicate label in offer choice"),
                    None => (),
                }
            }

            SessionElement::OfferChoice(offer_choice)
        },
    )(i)
}

fn inner_choice(i: &str) -> IResult<&str, (Label, Vec<SessionElement>)> {
    map(
        pair(alpha1, delimited(ws(tag("(")), st_inner, ws(tag(")")))),
        |(o1, o2)| (Label(o1.to_string()), o2),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::ast::{Atom, Fname, FunHead, Integer};
    use crate::st_parser::ast::SessionElement;
    use crate::st_parser::ast::SessionType::{NotST, Server};

    use super::*;

    #[test]
    fn simple_constructor_00() {
        assert_eq!(
            st_parse("'test'(_,server(!int.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Server(vec!(SessionElement::Send(Types::Single("int".to_owned())),))
                    ),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_constructor_01() {
        assert_eq!(
            st_parse("'test'(_,server(!int. ?string. end.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Server(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_constructor_02() {
        assert_eq!(
            st_parse("'test'(_,server(!int. ?string. end.)) -> (end.), []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Server(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![SessionElement::End],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_constructor_03() {
        assert_eq!(
            st_parse("'test'(_,server(!int. ?string. end.)) -> (end.), [SessionId: ?number. end.]"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Server(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![SessionElement::End],
                    binders: HashMap::from([(
                        Var("SessionId".to_owned()),
                        vec![
                            SessionElement::Receive(Types::Single("number".to_owned())),
                            SessionElement::End,
                        ]
                    )]),
                }
            ))
        );
    }

    #[test]
    fn simple_session_00() {
        assert_eq!(
            st_parse("'test'(_,session(!int.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Session(vec!(SessionElement::Send(Types::Single("int".to_owned())),))
                    ),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_session_01() {
        assert_eq!(
            st_parse("'test'(_,session(!int. ?string. end.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Session(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_session_02() {
        assert_eq!(
            st_parse("'test'(_,session(!int. ?string. end.)) -> (end.), []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Session(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![SessionElement::End],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn simple_session_03() {
        assert_eq!(
            st_parse(
                "'test'(_,session(!int. ?string. end.)) -> (end.), [SessionId: ?number. end.]"
            ),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(2)
                    },
                    st: vec!(
                        NotST,
                        Session(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        ))
                    ),
                    return_type: vec![SessionElement::End],
                    binders: HashMap::from([(
                        Var("SessionId".to_owned()),
                        vec![
                            SessionElement::Receive(Types::Single("number".to_owned())),
                            SessionElement::End,
                        ]
                    )]),
                }
            ))
        );
    }

    #[test]
    fn offer_choice_00() {
        assert_eq!(
            st_offer_choice("+{test(!int.)}"),
            Ok((
                "",
                SessionElement::OfferChoice(HashMap::from([(
                    Label("test".to_owned()),
                    vec![SessionElement::Send(Types::Single("int".to_owned()))]
                )]))
            ))
        );
    }

    #[test]
    fn offer_choice_01() {
        assert_eq!(
            st_parse("'test'(server( +{test(!int.)}.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(1)
                    },
                    st: vec!(Server(vec!(SessionElement::OfferChoice(HashMap::from([
                        (
                            Label("test".to_owned()),
                            vec![SessionElement::Send(Types::Single("int".to_owned()))]
                        )
                    ]))))),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn offer_choice_02() {
        assert_eq!(
            st_parse("'test'(server( +{test(!int. !int. ?string. end.)}.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(1)
                    },
                    st: vec!(Server(vec!(SessionElement::OfferChoice(HashMap::from([
                        (
                            Label("test".to_owned()),
                            vec![
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                                SessionElement::End,
                            ]
                        )
                    ]))))),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn offer_choice_03() {
        assert_eq!(
            st_parse("'test'(server( +{test(!int. !int. ?string. end.), alt(end.)}.)) -> _, []"),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(1)
                    },
                    st: vec!(Server(vec!(SessionElement::OfferChoice(HashMap::from([
                        (
                            Label("test".to_owned()),
                            vec![
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                                SessionElement::End,
                            ]
                        ),
                        (Label("alt".to_owned()), vec![SessionElement::End,]),
                    ]))))),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }

    #[test]
    fn make_choice_00() {
        assert_eq!(
            st_make_choice("&{test(!int.)}"),
            Ok((
                "",
                SessionElement::MakeChoice(vec![(
                    Label("test".to_owned()),
                    vec![SessionElement::Send(Types::Single("int".to_owned()))]
                )])
            ))
        );
    }

    #[test]
    fn make_choice_01() {
        assert_eq!(
            st_parse("'test'( server( &{ test( !int. ) } . ) ) -> _ , [  ]  "),
            Ok((
                "",
                SessionDef {
                    name: FunHead {
                        name: Fname(Atom("test".to_owned())),
                        arity: Integer(1)
                    },
                    st: vec!(Server(vec!(SessionElement::MakeChoice(vec![(
                        Label("test".to_owned()),
                        vec![SessionElement::Send(Types::Single("int".to_owned()))]
                    )])))),
                    return_type: vec![],
                    binders: HashMap::new(),
                }
            ))
        );
    }
    // TODO: Branch, Choice
}
