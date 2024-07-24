// Purpose: Take in a Core Erlang AST and extract session type specifications

use std::collections::HashMap;

use nom::Finish;

use crate::cerl_parser::{
    self,
    ast::{FunNameInner, Lit, LitInner},
};

use super::{
    ast::{SessionSpecDef, SessionSpecs},
    parser::st_parse,
};

pub fn session_spec_extractor(ast: &cerl_parser::ast::Module) -> Result<SessionSpecDef, String> {
    // Find all session attributes
    let mut session_spec_def: SessionSpecDef = SessionSpecDef(HashMap::new());
    for attribute in &ast.inner.attributes {
        if attribute.name.name == "session".to_owned() {
            match add_session_spec(&attribute.value) {
                Ok((fun_name, session_specs)) => {
                    if session_spec_def.0.contains_key(&fun_name) {
                        return Err(format!(
                            "FATAL: Duplicate -session for function {}. Exiting now...",
                            &fun_name
                        ));
                    } else {
                        session_spec_def.0.insert(fun_name.clone(), session_specs);
                    }
                }
                Err(err_val) => {
                    return Err(format!(
                        "Failed session spec extraction. Reason: {:?}",
                        err_val
                    ))
                }
            }
        }
        // TODO: Add support for custom type declarations?
    }
    Ok(session_spec_def)
}

fn add_session_spec(spec: &Lit) -> Result<(FunNameInner, SessionSpecs), String> {
    // Session type is wrapped within a list and then as the name of an atom

    // Unwrap the char list.
    let LitInner::Cons(_val_const) = spec.inner.clone() else {
        return Err("Expected cons for session spec".to_string());
    };

    // Flatten Cons list
    let val_const = flatten_cons(&spec.inner);

    // ASCII Decimal to string conversion...
    let mut st_string: String = String::new();
    for item in val_const {
        let LitInner::Int(char_to_decode) = item else {
            return Err(format!("ASCII decimal string to string conversion only works on integer literals. Found {:?}", item));
        };
        let char_to_decode_bytes = char_to_decode.to_be_bytes();
        let char_to_decode_last = char_to_decode_bytes.last().unwrap();
        let char_to_decode_last = &[*char_to_decode_last];
        let new_char = std::str::from_utf8(char_to_decode_last);
        if let Ok(new_char) = new_char {
            st_string.push_str(new_char)
        } else {
            return Err(format!(
                "ASCII Decimal to string conversion failure: {:?}",
                new_char.err()
            ));
        };
    }

    // Parse session type spec string
    let session_type_parsed: (FunNameInner, SessionSpecs) = match st_parse(&st_string).finish() {
        Ok((_, res)) => res,
        Err(e) => return Err(format!("Nom could not parse session type\n\n{}", e)),
    };
    Ok(session_type_parsed)
}

// TODO: Move this function to a common file.
// TODO: This might make a list too flat, let testing prove wheter it works or not.
// Flatten and throw away annotaitons
pub fn flatten_cons(cons: &LitInner) -> Vec<LitInner> {
    match cons {
        LitInner::Cons(cons_box) => {
            let (head, tail) = cons_box.as_ref();
            [flatten_cons(&head.inner), flatten_cons(&tail.inner)]
                .concat()
                .to_vec()
        }
        LitInner::Nil => vec![],
        X => vec![X.clone()],
    }
}
