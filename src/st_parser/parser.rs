use std::collections::HashMap;

use nom::{
    branch::alt,
    character::complete::alpha1,
    combinator::{map, value},
    multi::separated_list1,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use crate::cerl_parser::{
    ast::FunName,
    helpers::ws,
    terminals::{atom, var},
};

use super::ast::{ST, Label, SessionDef, SessionElement, SessionElementList, SessionType, Types};

use crate::st_parser::parser::SessionType::{New, NotST, Ongoing};

// TODO: Reuse of functions from cerl parser allows for comments and maybe annotations inside the ST which is odd
// Better to not reuse or rewrite so the core can be reused rather than the whole code.
// Doing for now to get a working prototype
pub fn st_parse(i: &str) -> IResult<&str, SessionDef, ErrorTree<&str>> {
    map(
        pair(
            atom,
            separated_list1(tag(";"), clause)
        ),
        |(fname, clauses)| {
            // TODO: WF Check: Duplicate var in st binders
            // TODO: WF Check: Consistent number of arguments

            SessionDef {
                name: FunName {
                    name: fname,
                    arity: clauses.first().unwrap().0.len().try_into().unwrap(),
                },
                st: clauses,
            }
        },
    )(i)
}

fn clause(i: &str) -> IResult<&str, ST, ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("(")),
            separated_list1(
                ws(tag(",")),
                alt((new_st, ongoing_st, value(NotST, ws(tag("_"))))),
            ),
            ws(tag(")")),
        ),
        |st| {
            // TODO: It should be possible to write this one more neatly... 
            ST(st)
        }
    )(i)
}

fn new_st(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("new")),
            delimited(ws(tag("(")), st_inner, ws(tag(")"))),
        )),
        |(_, o)| New(o),
    )(i)
}

fn ongoing_st(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("ongoing")),
            delimited(
                ws(tag("(")),
                st_inner,
                ws(tag(")")),
            ),
        )),
        |(_, o)| Ongoing(o),
    )(i)
}

fn st_inner(i: &str) -> IResult<&str, SessionElementList, ErrorTree<&str>> {
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
        |(o, _)| SessionElementList(o),
    )(i)
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_send(i: &str) -> IResult<&str, SessionElement, ErrorTree<&str>> {
    map(preceded(tag("!"), alpha1), |o: &str| {
        SessionElement::Send(Types::Single(o.to_string()))
    })(i)
}

fn st_receive(i: &str) -> IResult<&str, SessionElement, ErrorTree<&str>> {
    map(preceded(tag("?"), alpha1), |o: &str| {
        SessionElement::Receive(Types::Single(o.to_string()))
    })(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_make_choice(i: &str) -> IResult<&str, SessionElement, ErrorTree<&str>> {
    map(
        delimited(ws(tag("&{")), inner_choice, ws(tag("}"))),
        |(o1, o2)| SessionElement::MakeChoice(o1, o2),
    )(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_offer_choice(i: &str) -> IResult<&str, SessionElement, ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("+{")),
            separated_list1(ws(tag(",")), inner_choice),
            ws(tag("}")),
        ),
        |o| {
            let mut offer_choice = HashMap::new();

            for (label, elm) in o {
                if offer_choice.insert(label.clone(), elm.clone()).is_some() {
                    panic!("Duplicate label in offer choice")
                }
            }

            SessionElement::OfferChoice(offer_choice)
        },
    )(i)
}

fn inner_choice(i: &str) -> IResult<&str, (Label, SessionElementList), ErrorTree<&str>> {
    map(
        pair(alpha1, delimited(ws(tag("(")), st_inner, ws(tag(")")))),
        |(o1, o2)| (Label(o1.to_string()), o2),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::ast::{Atom, FunName, Var};
    use crate::st_parser::ast::SessionElement;
    use crate::st_parser::ast::SessionType::{New, NotST, Ongoing};

    use super::*;

    #[test]
    fn simple_constructor_00() {
        assert_eq!(
            st_parse("'test'(_,new(!int.)) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        New(SessionElementList(vec!(SessionElement::Send(
                            Types::Single("int".to_owned())
                        ),)))
                    ),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_constructor_01() {
        assert_eq!(
            st_parse("'test'(_,new(!int. ?string. end.)) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec![
                        NotST,
                        New(SessionElementList(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        )))
                    ],
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_constructor_02() {
        assert_eq!(
            st_parse("'test'(_,new(!int. ?string. end.)) -> (end.), []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        New(SessionElementList(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        )))
                    ),
                    return_type: SessionElementList(vec![SessionElement::End]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_constructor_03() {
        assert_eq!(
            st_parse("'test'(_,new(!int. ?string. end.)) -> ( end. ), [SessionId: ?number. end.]")
                .unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        New(SessionElementList(vec!(
                            SessionElement::Send(Types::Single("int".to_owned())),
                            SessionElement::Receive(Types::Single("string".to_owned())),
                            SessionElement::End,
                        )))
                    ),
                    return_type: SessionElementList(vec![SessionElement::End]),
                    binders: HashMap::from([(
                        Var("SessionId".to_owned()),
                        SessionElementList(vec![
                            SessionElement::Receive(Types::Single("number".to_owned())),
                            SessionElement::End,
                        ])
                    )]),
                }
            )
        );
    }

    #[test]
    fn simple_ongoing_00() {
        assert_eq!(
            st_parse("'test'(_,ongoing(!int. -> end. )) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        Ongoing(
                            SessionElementList(vec!(SessionElement::Send(Types::Single(
                                "int".to_owned()
                            )),)),
                            Some(SessionElementList(vec![SessionElement::End]))
                        )
                    ),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_ongoing_01() {
        assert_eq!(
            st_parse("'test'(_,ongoing(!int. ?string. -> end.)) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        Ongoing(
                            SessionElementList(vec!(
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                            )),
                            Some(SessionElementList(vec![SessionElement::End]))
                        )
                    ),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_ongoing_02() {
        assert_eq!(
            st_parse("'test'(_,ongoing(!int. ?string. -> end.)) -> (end.), []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        Ongoing(
                            SessionElementList(vec!(
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                            )),
                            Some(SessionElementList(vec![SessionElement::End]))
                        )
                    ),
                    return_type: SessionElementList(vec![SessionElement::End]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn simple_ongoing_03() {
        assert_eq!(
            st_parse(
                "'test'(_,ongoing(!int. ?string. -> end.)) -> (end.), [SessionId: ?number. end.]"
            )
            .unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    st: vec!(
                        NotST,
                        Ongoing(
                            SessionElementList(vec!(
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                            )),
                            Some(SessionElementList(vec![SessionElement::End]))
                        )
                    ),
                    return_type: SessionElementList(vec![SessionElement::End]),
                    binders: HashMap::from([(
                        Var("SessionId".to_owned()),
                        SessionElementList(vec![
                            SessionElement::Receive(Types::Single("number".to_owned())),
                            SessionElement::End,
                        ])
                    )]),
                }
            )
        );
    }

    #[test]
    fn offer_choice_00() {
        assert_eq!(
            st_offer_choice("+{test(!int.)}").unwrap(),
            (
                "",
                SessionElement::OfferChoice(HashMap::from([(
                    Label("test".to_owned()),
                    SessionElementList(vec![SessionElement::Send(Types::Single("int".to_owned()))])
                )]))
            )
        );
    }

    #[test]
    fn offer_choice_01() {
        assert_eq!(
            st_parse("'test'(new( +{test(!int.)}.)) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    st: vec!(New(SessionElementList(vec!(SessionElement::OfferChoice(
                        HashMap::from([(
                            Label("test".to_owned()),
                            SessionElementList(vec![SessionElement::Send(Types::Single(
                                "int".to_owned()
                            ))])
                        )])
                    ))))),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn offer_choice_02() {
        assert_eq!(
            st_parse("'test'(new( +{test(!int. !int. ?string. end.)}.)) -> _, []").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    st: vec!(New(SessionElementList(vec!(SessionElement::OfferChoice(
                        HashMap::from([(
                            Label("test".to_owned()),
                            SessionElementList(vec![
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Send(Types::Single("int".to_owned())),
                                SessionElement::Receive(Types::Single("string".to_owned())),
                                SessionElement::End,
                            ])
                        )])
                    ))))),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn offer_choice_03() {
        assert_eq!(
            st_parse("'test'(new( +{test(!int. !int. ?string. end.), alt(end.)}.)) -> _, []")
                .unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    st: vec!(New(SessionElementList(vec!(SessionElement::OfferChoice(
                        HashMap::from([
                            (
                                Label("test".to_owned()),
                                SessionElementList(vec![
                                    SessionElement::Send(Types::Single("int".to_owned())),
                                    SessionElement::Send(Types::Single("int".to_owned())),
                                    SessionElement::Receive(Types::Single("string".to_owned())),
                                    SessionElement::End,
                                ])
                            ),
                            (
                                Label("alt".to_owned()),
                                SessionElementList(vec![SessionElement::End,])
                            ),
                        ])
                    ))))),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }

    #[test]
    fn make_choice_00() {
        assert_eq!(
            st_make_choice("&{test(!int.)}").unwrap(),
            (
                "",
                SessionElement::MakeChoice(
                    Label("test".to_owned()),
                    SessionElementList(vec![SessionElement::Send(Types::Single("int".to_owned()))])
                )
            )
        );
    }

    #[test]
    fn make_choice_01() {
        assert_eq!(
            st_parse("'test'( new( &{ test( !int. ) } . ) ) -> _ , [  ]  ").unwrap(),
            (
                "",
                SessionDef {
                    name: FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    st: vec!(New(SessionElementList(vec!(SessionElement::MakeChoice(
                        Label("test".to_owned()),
                        SessionElementList(vec![SessionElement::Send(Types::Single(
                            "int".to_owned()
                        ))])
                    ))))),
                    return_type: SessionElementList(vec![]),
                    binders: HashMap::new(),
                }
            )
        );
    }
    // TODO: Branch, Choice
}
