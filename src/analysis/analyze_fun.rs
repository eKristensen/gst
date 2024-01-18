// We need to know what functions returns

// There may be user defined functions in which case we take the -spec for that function
// For any build in function we must write a function that can return the correct type

// TODO: What about functions with more than one possible return type? ... Hmm
// Ignored for now...

// The function information environment is not updated while checking the session-types.
// This environment is used for lookup when variables are assigned or session-types checked.

use std::{collections::HashMap, ops::Deref};

use crate::{
    cerl_parser::ast::{Atom, Expr, Exprs, FunHead, Lit, Var},
    st_parser::ast::SessionMode,
};

use super::{analyze_var::VarType, types::Types};

use crate::cerl_parser::ast::Exprs::Single;

// TODO: Function to be used for lookup
// Algorithm: First look in environment, no match=Check statically for hard-coded types of
// functions. They may depend on arguments. As may user-defined functions.

pub fn get_bif_fun_type(mod_name: &Exprs, call_name: &Exprs) -> Result<Types, String> {
    let (mod_name, call_name) = fun_name_extractor(mod_name, call_name);
    match (mod_name.as_str(), call_name.as_str()) {
        ("io", "format") => Ok(Types::Single("no_return".to_owned())),
        _ => Err(format!("no bif match on {:?}:{:?}", mod_name, call_name)),
    }
}

pub fn fun_name_extractor(mod_name: &Exprs, call_name: &Exprs) -> (String, String) {
    let Single(mod_name) = mod_name else { todo!() };
    let mod_name = *mod_name.clone();
    let Expr::Lit(Lit::Atom(Atom(mod_name))) = mod_name else {
        todo!()
    };

    let Single(call_name) = call_name else {
        todo!()
    };
    let call_name = *call_name.clone();
    let Expr::Lit(Lit::Atom(Atom(call_name))) = call_name else {
        todo!()
    };

    (mod_name, call_name)
}
