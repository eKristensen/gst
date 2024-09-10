// Purpose: Take in a Core Erlang AST and extract session type specifications

use std::collections::HashMap;

use nom::Finish;

use crate::{
    cerl_parser::{
        self,
        ast::{Atom, FunName, Lit},
    },
    contract_cerl::types::SessionTypesList,
};

use super::{
    ast::{SessionSpecDef, SessionSpecs},
    parser::{st_inner, st_parse},
};

pub fn mspec_extractor(ast: &cerl_parser::ast::Module) -> Result<Option<SessionTypesList>, String> {
    // Find mspec if any defined for module
    for attribute in &ast.attributes {
        let Atom(a_name) = &attribute.name.name;
        if *a_name == "mspec" {
            // Use st_inner to typecheck inner value
            // Session type is wrapped within a list and then as the name of an atom

            // Unwrap the char list.
            let Lit::Cons(val_const) = &attribute.value.inner else {
                return Err("Expected cons for mspec".to_string());
            };

            let st_string = decimal_string_decode(val_const)?;

            // Convert encoded string to string. Move out to common part.
            match st_inner(st_string.as_str()) {
                Ok((_, mspec)) => return Ok(Some(mspec)),
                Err(err_val) => {
                    return Err(format!("Failed mspec extraction. Reason: {:?}", err_val))
                }
            }
        }
    }
    Ok(None)
}

pub fn session_spec_extractor(ast: &cerl_parser::ast::Module) -> Result<SessionSpecDef, String> {
    // Find all session attributes
    let mut session_spec_def: SessionSpecDef = SessionSpecDef(HashMap::new());
    for attribute in &ast.attributes {
        let Atom(a_name) = &attribute.name.name;
        if *a_name == "session" {
            match add_session_spec(&attribute.value.inner) {
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

fn add_session_spec(spec: &Lit) -> Result<(FunName, SessionSpecs), String> {
    // Session type is wrapped within a list and then as the name of an atom

    // Unwrap the char list.
    let Lit::Cons(val_const) = spec else {
        return Err("Expected cons for session spec".to_string());
    };

    let st_string = decimal_string_decode(val_const)?;

    // Parse session type spec string
    let session_type_parsed: (FunName, SessionSpecs) = match st_parse(&st_string).finish() {
        Ok((residual_input, res)) if residual_input.is_empty() => res,
        Ok((residual_iniput, _)) => {
            return Err(format!(
                "Could not parse session type. Extra unknown input not allowed: {:?}",
                residual_iniput
            ))
        }
        Err(e) => return Err(format!("Nom could not parse session type\n\n{}", e)),
    };
    Ok(session_type_parsed)
}

fn decimal_string_decode(input: &Vec<Lit>) -> Result<String, String> {
    // ASCII Decimal to string conversion...
    let mut st_string: String = String::new();
    for item in input {
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
    Ok(st_string)
}
