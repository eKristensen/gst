// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use crate::cerl_parser::ast::{FunHead, Var};

#[derive(Debug, Clone, PartialEq)]
pub struct SessionDef {
    pub name: FunHead,
    pub st: Vec<SessionType>, // TODO: A map would be better, but a tuple must do for now.
    pub return_type: Vec<SessionElement>,
    pub binders: Vec<(Var, Vec<SessionElement>)>, // TODO: A map would be better but a tuple is easier for now.
}

// Label to differentiate branches in session types. They are assumed to be non-overlapping
// TODO: Non-overlapping assumption reconsider
// TODO Note assumption: Labels are atoms in the erlang program. It is impossible to distinguish atoms and labels from each other currently.
#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub String);

// TODO: Can constructors be nested? Would that make sense at all?
#[derive(Debug, Clone, PartialEq)]
pub enum SessionType {
    NotST, // Needed as a placeholder for the initial session spec to be able to count the variable arguments
    Server(Vec<SessionElement>), // Constructor for new session
    Session(Vec<SessionElement>), // "ongoing" session
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
    MakeChoice(Vec<(Label, Vec<SessionElement>)>), // TODO: Check/Ask Marco: Should make choice be a vector?
    OfferChoice(Vec<(Label, Vec<SessionElement>)>),
    End, // TODO: Ask Marco: Can end be consumed?
}
