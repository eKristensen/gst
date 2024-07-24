// Represent session types
// The choice is to write session types as a string in core erlang
// and then parse the session types from the string after core
// erlang has been parsed as needed
// const in erlang is not a good way to represent session types directly in core erlang.

// Based on https://github.com/gertab/ElixirST#session-types-in-elixir

use std::collections::HashMap;

use crate::contract_cerl::types::SessionTypesList;

use crate::cerl_parser::ast::FunNameInner;

// TODO: Allow for more flexible spec annotations. Almost completely static specification assumed now.
#[derive(Debug, Clone, PartialEq)]
pub struct SessionSpecDef(pub HashMap<FunNameInner, SessionSpecs>);

#[derive(Debug, Clone, PartialEq)]
pub struct SessionSpecs(pub Vec<SessionSpec>);

#[derive(Debug, Clone, PartialEq)]
pub struct SessionSpec(pub Vec<SessionSpecElm>);

// TODO: Can constructors be nested? Would that make sense at all?
#[derive(Debug, Clone, PartialEq)]
pub enum SessionSpecElm {
    BasePlaceholder, // Needed as a placeholder for the initial session spec to be able to count the variable arguments
    NewSpec(SessionTypesList), // Constructor for new session
    ConsumeSpec(SessionTypesList), // "ongoing" session
                     // Development notes ( TODO move somewhere else)
                     // 10. april 2024: binders removed
                     // Idea: Ongoing checking consistent: When ongoing is constructed copy the expected return type from binders
                     // This idea would also change how to accept an env after type checking.
                     // Or maybe it is better the other way around: Ongoing adds to the binders? Hmm. Properly not, with this option
                     // Name clashes can happen. So that is not going to happen.
                     // Nope, actually I need to look it up in the end no matter what.
                     // I cannot construct ongoing with a sensible return type inside the function body as
                     // I would have to look forward to construct the session type. The information I need is the variable name
                     // that the session type is bound to, and even then it might be bad to rely on keeping that information
}
