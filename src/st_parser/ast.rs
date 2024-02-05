// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use std::{collections::HashMap, fmt};

use crate::cerl_parser::ast::{FunName, Var};

// TODO Multi-option function like in -spec cannot be represented right now.
// The -session is assumed to only have a single case for now.
#[derive(Debug, Clone, PartialEq)]
pub struct SessionDef {
    pub name: FunName,
    pub st: Vec<SessionType>,
    pub return_type: SessionElementList,
    pub binders: HashMap<Var, SessionElementList>,
}

// Label to differentiate branches in session types. They are assumed to be non-overlapping
// TODO: Non-overlapping assumption reconsider
// TODO Note assumption: Labels are atoms in the erlang program. It is impossible to distinguish atoms and labels from each other currently.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(pub String);

// TODO: Can constructors be nested? Would that make sense at all?
#[derive(Debug, Clone, PartialEq)]
pub enum SessionType {
    NotST, // Needed as a placeholder for the initial session spec to be able to count the variable arguments
    New(SessionElementList), // Constructor for new session
    Ongoing(SessionElementList, Option<SessionElementList>), // "ongoing" session: (ST,ST')
           // Development notes ( TODO move somewhere else)
           // Idea: Ongoing checking consistent: When ongoing is constructed copy the expected return type from binders
           // This idea would also change how to accept an env after type checking.
           // Or maybe it is better the other way around: Ongoing adds to the binders? Hmm. Properly not, with this option
           // Name clashes can happen. So that is not going to happen.
           // Nope, actually I need to look it up in the end no matter what.
           // I cannot construct ongoing with a sensible return type inside the function body as
           // I would have to look forward to construct the session type. The information I need is the variable name
           // that the session type is bound to, and even then it might be bad to rely on keeping that information
}

// https://github.com/gertab/ElixirST/blob/75d098f51df40b5ff1022c7dc56a695b0f3da9d9/lib/elixirst/session_type.ex#L122
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Types {
    Single(String),
    Tuple(Vec<Types>),
    // Cons(Vec<Types>), // TODO implement Cons
}

#[derive(Debug, Clone, PartialEq)]
pub enum SessionElement {
    Send(Types),
    Receive(Types),
    MakeChoice(Label, SessionElementList),
    OfferChoice(HashMap<Label, SessionElementList>),
    End, // End is never consumed
}

// TODO: Evaluate better options or better use for a type alias. Maybe a different way to do it would work better?
#[derive(Debug, Clone, PartialEq)]
pub struct SessionElementList(pub Vec<SessionElement>);

impl fmt::Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Types::Single(res) => write!(f, "{}", res),
            Types::Tuple(res) => {
                // TODO Generalize instead of copy-paste
                let mut out: String = "".to_string();
                res.iter().fold(true, |first, elem| {
                    if !first {
                        out.push_str(", ");
                    }
                    out.push_str(format!("{}", elem).as_str());
                    false
                });
                write!(f, "{{{}}}", out)
            }
        }
    }
}

impl fmt::Display for SessionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SessionType::NotST => write!(f, "_"),
            SessionType::New(res) => {
                write!(f, "new({})", res)
            }
            SessionType::Ongoing(res1, res2) => {
                // TODO Generalize instead of copy-paste
                let res2 = match res2 {
                    Some(res2) => res2.clone(), // TODO Stupid clone
                    None => SessionElementList(vec![]),
                };
                write!(f, "ongoing({}, {})", res1, res2)
            }
        }
    }
}

impl fmt::Display for SessionElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out: String = "".to_string();
        self.0.iter().fold(true, |first, elem| {
            if !first {
                out.push_str(", ");
            }
            out.push_str(format!("{}", elem).as_str());
            false
        });
        // TODO: Does this syntax match what I'm parsing?
        write!(f, "{}", out)
    }
}

// TODO: Self-feeding test: Can I parse something print and parse and get the same result? That should be the case.
impl fmt::Display for SessionElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SessionElement::Send(res) => write!(f, "!{}", res),
            SessionElement::Receive(res) => write!(f, "?{}", res),
            SessionElement::MakeChoice(label, st) => {
                write!(f, "&{{{}, {}}}", label, st)
            }
            SessionElement::OfferChoice(res) => write!(f, "{:?}", res), // TODO: Pretty print OfferChoice
            SessionElement::End => write!(f, "end."),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
