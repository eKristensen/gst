// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use crate::cerl_parser::ast::FunHead;

#[derive(Debug, Clone, PartialEq)]
pub struct Session {
    pub name: FunHead,
    pub st: Vec<SessionMode>, // TODO: A map would be better, but a tuple must do for now.
}

// TODO: Should accept erlang and custom types. A starting point could be
// https://github.com/gertab/ElixirST/blob/75d098f51df40b5ff1022c7dc56a695b0f3da9d9/lib/elixirst/session_type.ex#L122
// Easier to get started without having to worry about type names as well.
// Disadvantage is that possible type checks are very limited to Equality in name
#[derive(Debug, Clone, PartialEq)]
pub struct ElmType(pub String);

// Label to differentiate branches in session types. They are assumed to be non-overlapping
// TODO: Non-overlapping assumption reconsider
#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub String);

// TODO: SessionMode == Bad name, very bad name indeed
#[derive(Debug, Clone, PartialEq)]
pub enum SessionMode {
    NotST,
    Fresh(Vec<SessionType>),
    Ongoing(Vec<SessionType>, Vec<SessionType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SessionType {
    Send(ElmType),
    Receive(ElmType),
    Branch(Vec<(Label, SessionType)>),
    Choice(Vec<(Label, SessionType)>),
}
