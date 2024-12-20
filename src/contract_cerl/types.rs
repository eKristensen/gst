// Common Erlang type representation
// Used (at least) by analysis, spec_extractor and st_parser
// Types at not known by the cerl_parser, but used almost everywhere after parsing cerl

use std::{collections::BTreeMap, fmt, rc::Rc};

use crate::cerl_parser::ast::{Atom, Var};

// Type support is limited to the ones below.
// TODO: Allow generic/new types.
// TODO: Remember to test all cases, i.e. to include all possible types in tests to ensure they work as intended.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BaseType {
    Atom(Rc<Atom>), // Atom is named as it is a constant that can be checked statically
    Pid,
    Reference,
    Integer,
    Float,
    Boolean,
    Cons(Vec<BaseType>),
    Tuple(Vec<BaseType>),
    Char,
    String,
    Any,                                              // Aka "term"
    Dynamic,                                          // The gradual type to be used when casting
    Fun(Option<Vec<BaseType>>, Option<Rc<BaseType>>), // For anonymous functions
    List, // TODO: Differentiate between list types and include maybe improper and improper lists.
    Map,  // TODO: Differentiate between different kinds of maps.
}
// Label to differentiate branches in session types. They are assumed to be non-overlapping
// TODO: Non-overlapping assumption reconsider
// TODO Note assumption: Labels are atoms in the erlang program. It is impossible to distinguish atoms and labels from each other currently.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Label(pub String); // To differentiate between atoms and labels in the type system.

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SessionTypesList(pub Vec<SessionType>);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ChoiceType {
    Make,
    Offer,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SessionType {
    Send(BaseType),
    Receive(BaseType),
    // TODO: Add wellformed check: No elements after MakeChoice/OfferChoice/End
    // Why list? Easier to work with in Rust to avoid Boxing.
    Choice(ChoiceType, BTreeMap<Label, SessionTypesList>),
    State(Rc<Atom>), // for mspec to support gen server plus as a state machine.
    End,             // End to close session
    Cut,             // For consume, stop consuming here, "cut" session type here
    Var(Rc<Var>),    // Recursion variable binder
    Rec(Rc<Var>),    // Recursion
}

// TODO: Reimplement new Display functions for the types above for pretty printing.
// impl fmt::Display for Types {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Types::Single(res) => write!(f, "{}", res),
//             Types::Tuple(res) => {
//                 // TODO Generalize instead of copy-paste
//                 let mut out: String = "".to_string();
//                 res.iter().fold(true, |first, elem| {
//                     if !first {
//                         out.push_str(", ");
//                     }
//                     out.push_str(format!("{}", elem).as_str());
//                     false
//                 });
//                 write!(f, "{{{}}}", out)
//             }
//         }
//     }
// }

// impl fmt::Display for SessionType {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             SessionType::NotST => write!(f, "_"),
//             SessionType::New(res) => {
//                 write!(f, "new({})", res)
//             }
//             SessionType::Consume(res) => {
//                 write!(f, "consume({},)", res)
//             }
//         }
//     }
// }

// impl fmt::Display for SessionElementList {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let mut out: String = "".to_string();
//         self.0.iter().fold(true, |first, elem| {
//             if !first {
//                 out.push_str(", ");
//             }
//             out.push_str(format!("{}", elem).as_str());
//             false
//         });
//         // TODO: Does this syntax match what I'm parsing?
//         write!(f, "{}", out)
//     }
// }

// // TODO: Self-feeding test: Can I parse something print and parse and get the same result? That should be the case.
// impl fmt::Display for SessionElement {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             SessionElement::Send(res) => write!(f, "!{}", res),
//             SessionElement::Receive(res) => write!(f, "?{}", res),
//             SessionElement::MakeChoice(label, st) => {
//                 write!(f, "&{{{}, {}}}", label, st)
//             }
//             SessionElement::OfferChoice(res) => write!(f, "{:?}", res), // TODO: Pretty print OfferChoice
//             SessionElement::End => write!(f, "end."),
//         }
//     }
// }

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
