use std::collections::BTreeMap;

use nom::{
    branch::alt,
    character::complete::alpha1,
    combinator::{map, value},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use crate::{
    cerl_parser::ast::FunName,
    contract_cerl::types::{BaseType, Label, SessionType, SessionTypesList},
};
use crate::{cerl_parser::tokeniser::var, st_parser::ast::SessionSpecElm::ConsumeSpec};
use crate::{
    cerl_parser::{helpers::wsa, tokeniser::atom},
    st_parser::ast::SessionSpecElm::BasePlaceholder,
};
use crate::{contract_cerl::types::ChoiceType, st_parser::ast::SessionSpecElm::NewSpec};

use super::ast::{SessionSpec, SessionSpecElm, SessionSpecs};

// TODO: Reuse of functions from cerl parser allows for comments and maybe annotations inside the ST which is odd
// Better to not reuse or rewrite so the core can be reused rather than the whole code.
// Doing for now to get a working prototype
// WSA OK
pub fn st_parse(i: &str) -> IResult<&str, (FunName, SessionSpecs), ErrorTree<&str>> {
    map(
        pair(
            atom,
            map(separated_list1(wsa(tag(";")), clause), SessionSpecs),
        ),
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

// WSA OK
fn clause(i: &str) -> IResult<&str, SessionSpec, ErrorTree<&str>> {
    map(
        delimited(
            wsa(tag("(")),
            separated_list0(
                wsa(tag(",")),
                alt((
                    new_spec,
                    consume_spec,
                    value(BasePlaceholder, wsa(tag("_"))),
                )),
            ),
            wsa(tag(")")),
        ),
        SessionSpec,
    )(i)
}

// WSA OK
fn new_spec(i: &str) -> IResult<&str, SessionSpecElm, ErrorTree<&str>> {
    map(
        preceded(
            tag("new"),
            delimited(wsa(tag("(")), st_inner, wsa(tag(")"))),
        ),
        NewSpec,
    )(i)
}

// WSA OK
fn consume_spec(i: &str) -> IResult<&str, SessionSpecElm, ErrorTree<&str>> {
    map(
        preceded(
            tag("consume"),
            delimited(wsa(tag("(")), st_inner, wsa(tag(")"))),
        ),
        ConsumeSpec,
    )(i)
}

// WSA OK
pub fn st_inner(i: &str) -> IResult<&str, SessionTypesList, ErrorTree<&str>> {
    map(
        separated_list0(
            wsa(tag(".")),
            alt((
                map(preceded(tag("!"), base_type), SessionType::Send),
                map(preceded(tag("?"), base_type), SessionType::Receive),
                st_choice,
                value(SessionType::End, wsa(tag("end"))),
                value(SessionType::Cut, wsa(tag("-"))),
                mspec_state,
                map(var, SessionType::Var),
                // TODO: Well formed check for recusion. No longer parses the set of rec and
                // the inner at once.
                map(preceded(wsa(tag("rec")), var), SessionType::Rec),
            )),
        ),
        SessionTypesList,
    )(i)
}

// WSA OK
fn base_type(i: &str) -> IResult<&str, BaseType, ErrorTree<&str>> {
    alt((
        map(atom, BaseType::Atom),
        value(BaseType::Pid, wsa(tag("pid"))),
        value(BaseType::Reference, wsa(tag("reference"))),
        value(BaseType::Integer, wsa(tag("integer"))),
        value(BaseType::Float, wsa(tag("float"))),
        value(BaseType::Boolean, wsa(tag("boolean"))),
        value(BaseType::String, wsa(tag("string"))),
        // TODO: How to do cons and tuple in a sensible way???
    ))(i)
}

// WSA OK
// TODO: ElixirST has ? or ! on labels. Needed or not?
fn st_choice(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    // TODO: Currently just like st_offer_choice make avoid duplicate code
    map(
        pair(
            alt((
                value(ChoiceType::Make, tag("+")),
                value(ChoiceType::Offer, tag("&")),
            )),
            delimited(
                wsa(tag("{")),
                separated_list1(wsa(tag(",")), inner_choice),
                wsa(tag("}")),
            ),
        ),
        |(o1, o2)| {
            let mut choices = BTreeMap::new();

            for (label, elm) in o2 {
                if choices.insert(label.clone(), elm.clone()).is_some() {
                    // TODO: Can i get rid of the panic here?
                    panic!("Duplicate label in offer choice")
                }
            }

            SessionType::Choice(o1, choices)
        },
    )(i)
}

// WSA OK
fn inner_choice(i: &str) -> IResult<&str, (Label, SessionTypesList), ErrorTree<&str>> {
    map(
        tuple((wsa(alpha1), wsa(tag(":")), st_inner)),
        |(o1, _, o2)| (Label(o1.to_string()), o2),
    )(i)
}

// WSA OK
fn mspec_state(i: &str) -> IResult<&str, SessionType, ErrorTree<&str>> {
    map(
        delimited(wsa(tag("<")), atom, wsa(tag(">"))),
        SessionType::State,
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
            st_parse("'test'(_,new(!integer))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,new( !integer. ?float. end  ))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,new(!integer. ?float. end)) ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,new(!integer. ?float. end)) ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,consume(!integer ))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,consume(!integer. ?float))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,consume(!integer. ?float))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_parse("'test'(_,consume(!integer. ?float))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
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
            st_choice("&{test:!integer}").unwrap(),
            (
                "",
                SessionType::Choice(
                    ChoiceType::Offer,
                    BTreeMap::from([(
                        Label("test".to_owned()),
                        SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                    )])
                )
            )
        );
    }

    #[test]
    fn offer_choice_01() {
        assert_eq!(
            st_parse("'test'(new( &{test:!integer}))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::Choice(
                            ChoiceType::Offer,
                            BTreeMap::from([(
                                Label("test".to_owned()),
                                SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                            )])
                        )
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn offer_choice_02() {
        assert_eq!(
            st_parse("'test'(new( &{test:!integer. !integer . ?float. end}))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::Choice(
                            ChoiceType::Offer,
                            BTreeMap::from([(
                                Label("test".to_owned()),
                                SessionTypesList(vec![
                                    SessionType::Send(BaseType::Integer),
                                    SessionType::Send(BaseType::Integer),
                                    SessionType::Receive(BaseType::Float),
                                    SessionType::End,
                                ])
                            )])
                        )
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn offer_choice_03() {
        assert_eq!(
            st_parse("'test'(new( &{test:!integer. !integer. ?float. end, alt: end}))").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::Choice(
                            ChoiceType::Offer,
                            BTreeMap::from([
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
                            ])
                        )
                    ))))))),
                )
            )
        );
    }

    #[test]
    fn make_choice_00() {
        assert_eq!(
            st_choice("+{test: !integer}").unwrap(),
            (
                "",
                SessionType::Choice(
                    ChoiceType::Make,
                    BTreeMap::from([(
                        Label("test".to_owned()),
                        SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                    )])
                )
            )
        );
    }

    #[test]
    fn make_choice_01() {
        assert_eq!(
            st_parse("'test'( new( +{ test: !integer }  ) )   ").unwrap(),
            (
                "",
                (
                    FunName {
                        name: Atom("test".to_owned()).into(),
                        arity: 1,
                    },
                    SessionSpecs(vec!(SessionSpec(vec!(NewSpec(SessionTypesList(vec!(
                        SessionType::Choice(
                            ChoiceType::Make,
                            BTreeMap::from([(
                                Label("test".to_owned()),
                                SessionTypesList(vec![SessionType::Send(BaseType::Integer)])
                            )])
                        )
                    ))))))),
                )
            )
        );
    }
    // TODO: Branch, Choice
}
