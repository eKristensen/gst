// We need to know what functions returns

// There may be user defined functions in which case we take the -spec for that function
// For any build in function we must write a function that can return the correct type

// TODO: What about functions with more than one possible return type? ... Hmm
// Ignored for now...

// The function information environment is not updated while checking the session-types.
// This environment is used for lookup when variables are assigned or session-types checked.

use crate::{
    cerl_parser::ast::{Atom, FunCall, FunKind},
    st_parser::ast::Types,
};

// TODO: Function to be used for lookup
// Algorithm: First look in environment, no match=Check statically for hard-coded types of
// functions. They may depend on arguments. As may user-defined functions.

pub fn get_bif_fun_type(call: &FunCall) -> Result<Types, String> {
    let FunCall {
        kind: FunKind::Call(kind),
        name: Atom(name),
    } = call
    else {
        return Err(format!("Only call supported so far"));
    };
    // TODO: Does as_str imply a clone ?
    match (kind.as_str(), name.as_str()) {
        ("io", "format") => Ok(Types::Single("no_return".to_owned())),
        _ => Err(format!("no bif match on {:?}", call)),
    }
}
