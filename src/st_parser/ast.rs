// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use std::collections::HashMap;

use crate::cerl_parser::ast::{FunHead, Var};

// TODO Multi-option function like in -spec cannot be represented right now.
// The -session is assumed to only have a single case for now.
#[derive(Debug, Clone, PartialEq)]
pub struct SessionDef {
    pub name: FunHead,
    pub st: Vec<SessionType>, // TODO: A map would be better, but a tuple must do for now.
    pub return_type: Vec<SessionElement>,
    pub binders: HashMap<Var, Vec<SessionElement>>,
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
    New(Vec<SessionElement>), // Constructor for new session
    Ongoing(Vec<SessionElement>, Option<Vec<SessionElement>>), // "ongoing" session: (ST,ST')
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
    Cons(Vec<Types>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SessionElement {
    Send(Types),
    Receive(Types),
    MakeChoice(Label, Vec<SessionElement>),
    OfferChoice(HashMap<Label, Vec<SessionElement>>),
    End, // End is never consumed
}
