// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use crate::{analysis::types::Types, cerl_parser::ast::FunHead};

#[derive(Debug, Clone, PartialEq)]
pub struct Session {
    pub name: FunHead,
    pub st: Vec<SessionMode>, // TODO: A map would be better, but a tuple must do for now.
}

// Label to differentiate branches in session types. They are assumed to be non-overlapping
// TODO: Non-overlapping assumption reconsider
#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub String);

// TODO: SessionMode == Bad name, very bad name indeed
#[derive(Debug, Clone, PartialEq)]
pub enum SessionMode {
    NotST, // This is odd in combination with VarType that puts SessionMode within a ST and has Base type.. TODO: Fixme
    Fresh(bool, Vec<SessionType>), // Bool is whether the session is initialized yet or not.
    Ongoing(Vec<SessionType>, Vec<SessionType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SessionType {
    Send(Types),
    Receive(Types),
    Branch(Vec<(Label, SessionType)>),
    Choice(Vec<(Label, SessionType)>),
}
