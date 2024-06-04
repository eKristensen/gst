// Purpose: Take in a Core Erlang AST and extract session type specifications

use std::collections::HashMap;

use nom::Finish;

use crate::cerl_parser::{
    self,
    ast::{Atom, FunName, Lit},
};

use super::{
    ast::{SessionSpecDef, SessionSpecs},
    parser::st_parse,
};

pub fn session_spec_extractor(ast: &cerl_parser::ast::Module) -> Result<SessionSpecDef, String> {
    // Find all session attributes
    let mut session_spec_def: SessionSpecDef = SessionSpecDef(HashMap::new());
    for attribute in &ast.attributes {
        if attribute.name == Atom("session".to_owned()) {
            let new_session_spec = add_session_spec(&attribute.value);
            if new_session_spec.is_ok() {
                let (fun_name, session_specs) = new_session_spec.unwrap();
                if session_spec_def.0.contains_key(&fun_name) {
                    return Err(format!(
                        "FATAL: Duplicate -session for function {}. Exiting now...",
                        &fun_name
                    ));
                } else {
                    session_spec_def.0.insert(fun_name.clone(), session_specs);
                }
            } else {
                println!(
                    "Warning: Failed session spec extraction. Reason: {:?}",
                    new_session_spec.err()
                )
            }
        }
        // TODO: Add support for custom type declarations?
    }
    Ok(session_spec_def)
}

fn add_session_spec(spec: &Lit) -> Result<(FunName, SessionSpecs), String> {
    // Session type is wrapped within a list and then as the name of an atom

    // Unwrap the char list.
    let Lit::Cons(val_const) = spec else {
        return Err("Expected cons for session spec".to_string());
    };

    // ASCII Decimal to string conversion...
    let mut st_string: String = String::new();
    for item in val_const {
        let Lit::Int(char_to_decode) = item else {
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
    let session_type_parsed: (FunName, SessionSpecs) = match st_parse(&st_string).finish() {
        Ok((_, res)) => res,
        Err(e) => return Err(format!("Nom could not parse session type\n\n{}", e)),
    };
    Ok(session_type_parsed)
}
