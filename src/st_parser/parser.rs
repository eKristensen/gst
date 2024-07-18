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

use crate::st_parser::ast::SessionSpecElm::BasePlaceholder;
use crate::st_parser::ast::SessionSpecElm::ConsumeSpec;
use crate::st_parser::ast::SessionSpecElm::NewSpec;
use crate::{
    cerl_parser::{ast::FunName, helpers::ws, terminals::atom},
    contract_cerl::types::{BaseType, Label, SessionType, SessionTypesList},
};

use super::ast::{SessionSpec, SessionSpecElm, SessionSpecs};

// TODO: Reuse of functions from cerl parser allows for comments and maybe annotations inside the ST which is odd
// Better to not reuse or rewrite so the core can be reused rather than the whole code.
// Doing for now to get a working prototype
pub fn st_parse(i: &str) -> IResult<&str, (FunName, SessionSpecs), ErrorTree<&str>> {
    map(
        pair(atom, map(separated_list1(tag(";"), clause), SessionSpecs)),
        |(fname, clauses)| {
            // TODO: WF Check: Duplicate var in st binders
            // TODO: WF Check: Consistent number of arguments
            (
                FunName {
                    name: fname,
                    arity: clauses.0.first().unwrap().0.len(),
                },
                clauses,
            )
        },
    )(i)
}

fn clause(i: &str) -> IResult<&str, SessionSpec, ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("(")),
            separated_list1(
                ws(tag(",")),
                alt((new_spec, consume_spec, value(BasePlaceholder, ws(tag("_"))))),
            ),
            ws(tag(")")),
        ),
        SessionSpec,
    )(i)
}

fn new_spec(i: &str) -> IResult<&str, SessionSpecElm, ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("new")),
            delimited(ws(tag("(")), st_inner, ws(tag(")"))),
        )),
        |(_, o)| NewSpec(o),
    )(i)
}

fn consume_spec(i: &str) -> IResult<&str, SessionSpecElm, ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("consume")),
            delimited(ws(tag("(")), st_inner, ws(tag(")"))),
        )),
        |(_, o)| ConsumeSpec(o),
    )(i)
}

fn st_inner(i: &str) -> IResult<&str, SessionTypesList, ErrorTree<&str>> {
    map(
        pair(
            separated_list1(
                ws(tag(".")),
                alt((
                    st_send,
                    st_receive,
                    st_make_choice,
                    st_offer_choice,
                    value(SessionType::End, tag("end")),
                )),
            ), // TODO: Add branch and choice
            tag("."),
        ),
        |(o, _)| SessionTypesList(o),
    )(i)
}

// TODO: Why does it not work when rewritten to map(pair(..),|(_,o) [return here]) ??
fn st_send(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    map(preceded(tag("!"), base_type), SessionType::Send)(i)
}

fn st_receive(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    map(preceded(tag("?"), base_type), SessionType::Receive)(i)
}

fn base_type(i: &str) -> IResult<&str, BaseType, ErrorTree<&str>> {
    alt((
        map(atom, BaseType::Atom),
        value(BaseType::Pid, tag("pid")),
        value(BaseType::Reference, tag("reference")),
        value(BaseType::Integer, tag("integer")),
        value(BaseType::Float, tag("float")),
        value(BaseType::Boolean, tag("boolean")),
        value(BaseType::String, tag("string")),
        // TODO: How to do cons and tuple in a sensible way???
    ))(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_make_choice(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    // TODO: Currently just like st_offer_choice make avoid duplicate code
    map(
        delimited(
            ws(tag("&{")),
            separated_list1(ws(tag(",")), inner_choice),
            ws(tag("}")),
        ),
        |o| {
            let mut offer_choice = HashMap::new();

            for (label, elm) in o {
                if offer_choice.insert(label.clone(), elm.clone()).is_some() {
                    // TODO: Can i get rid of the panic here?
                    panic!("Duplicate label in offer choice")
                }
            }

            SessionType::MakeChoice(offer_choice)
        },
    )(i)
}

// TODO: Avoid direct OK return
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_offer_choice(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
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
                    // TODO: Can i get rid of the panic here?
                    panic!("Duplicate label in offer choice")
                }
            }

            SessionType::OfferChoice(offer_choice)
        },
    )(i)
}

fn inner_choice(i: &str) -> IResult<&str, (Label, SessionTypesList), ErrorTree<&str>> {
    map(
        pair(alpha1, delimited(ws(tag("(")), st_inner, ws(tag(")")))),
        |(o1, o2)| (Label(o1.to_string()), o2),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::cerl_parser::ast::Atom;

    use super::*;

    // TODO: Add multi-clause tests (finally the type system shows what tests that I am missing, the implicit goal all along!)

    #[test]
    fn simple_constructor_00() {
        assert_eq!(
            st_parse("'test'(_,new(!integer.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        NewSpec(SessionTypesList(vec!(SessionType::Send(BaseType::Integer))),)
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_constructor_01() {
        assert_eq!(
            st_parse("'test'(_,new( !integer. ?float. end.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        NewSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                            SessionType::End,
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_constructor_02() {
        assert_eq!(
            st_parse("'test'(_,new(!integer. ?float. end.)) ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        NewSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                            SessionType::End,
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_constructor_03() {
        assert_eq!(
            st_parse("'test'(_,new(!integer. ?float. end.)) ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        NewSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                            SessionType::End,
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_consume_00() {
        assert_eq!(
            st_parse("'test'(_,consume(!integer. ))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        ConsumeSpec(SessionTypesList(
                            vec!(SessionType::Send(BaseType::Integer),)
                        ))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_consume_01() {
        assert_eq!(
            st_parse("'test'(_,consume(!integer. ?float.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        ConsumeSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_consume_02() {
        assert_eq!(
            st_parse("'test'(_,consume(!integer. ?float.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        ConsumeSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn simple_consume_03() {
        assert_eq!(
            st_parse("'test'(_,consume(!integer. ?float.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 2,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(
                        BasePlaceholder,
                        ConsumeSpec(SessionTypesList(vec!(
                            SessionType::Send(BaseType::Integer),
                            SessionType::Receive(BaseType::Float),
                        )))
                    )))),
                )
            )
        );
    }

    #[test]
    fn offer_choice_00() {
        assert_eq!(
            st_offer_choice("+{test(!integer.)}").unwrap(),
            (
                "",
                SessionType::OfferChoice(HashMap::from([(
                    Label("test".to_owned()),
                    SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                )]))
            )
        );
    }

    #[test]
    fn offer_choice_01() {
        assert_eq!(
            st_parse("'test'(new( +{test(!integer.)}.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::OfferChoice(HashMap::from([(
                            Label("test".to_owned()),
                            SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                        )]))
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn offer_choice_02() {
        assert_eq!(
            st_parse("'test'(new( +{test(!integer. !integer. ?float. end.)}.))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::OfferChoice(HashMap::from([(
                            Label("test".to_owned()),
                            SessionTypesList(vec![
                                SessionType::Send(BaseType::Integer),
                                SessionType::Send(BaseType::Integer),
                                SessionType::Receive(BaseType::Float),
                                SessionType::End,
                            ])
                        )]))
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn offer_choice_03() {
        assert_eq!(
            st_parse("'test'(new( +{test(!integer. !integer. ?float. end.), alt(end.)}.))")
                .unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::OfferChoice(HashMap::from([
                            (
                                Label("test".to_owned()),
                                SessionTypesList(vec![
                                    SessionType::Send(BaseType::Integer),
                                    SessionType::Send(BaseType::Integer),
                                    SessionType::Receive(BaseType::Float),
                                    SessionType::End,
                                ])
                            ),
                            (
                                Label("alt".to_owned()),
                                SessionTypesList(vec![SessionType::End,])
                            ),
                        ]))
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn make_choice_00() {
        assert_eq!(
            st_make_choice("&{test(!integer.)}").unwrap(),
            (
                "",
                SessionType::MakeChoice(HashMap::from([(
                    Label("test".to_owned()),
                    SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                )]))
            )
        );
    }

    #[test]
    fn make_choice_01() {
        assert_eq!(
            st_parse("'test'( new( &{ test( !integer. ) } . ) )   ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::MakeChoice(HashMap::from([(
                            Label("test".to_owned()),
                            SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                        )]))
                    ))))))),
                )
            )
        );
    }
    // TODO: Branch, Choice
}
