// Check ST annotation matches function body

use std::collections::HashMap;

use nom::Finish;

use crate::{
    cerl_parser::ast::{Atom, FunDef, FunName, Lit, Module},
    st_parser::{
        ast::{SessionDef, Types},
        parser::st_parse,
    },
};

use super::{env::FunEnv, wellformed::check_wf};

// Extract relevant parts of the core erlang module for analysis
pub fn init_module_env(m: Module) -> HashMap<FunName, FunEnv> {
    // Info: Name of module and module exports not relevant yet
    let mut env: HashMap<FunName, FunEnv> = HashMap::new();

    // Use attributes to get spec and session
    for attribute in &m.attributes {
        let Atom(a_name) = &attribute.name;
        if a_name.eq("spec") {
            add_spec(&mut env, &attribute.value)
        }
        if a_name.eq("session") {
            add_session(&mut env, &attribute.value)
        }
        // TODO: Ignore custom type declarations for now. Maybe reconsider later?
    }

    // Use body to get body of functions
    for (fun_head, fun_body) in &m.body {
        add_body(&mut env, fun_head, fun_body);
    }

    // For all functions: Decode "spec" to something that is consumable.
    // Maybe better to "just" decode them "at once"?
    // How to deal with split spec definitions? Multi-case spec

    // Finally check data is well formed
    let wf_res = check_wf(m, &env);
    if wf_res.is_err() {
        panic!("Well-formed check failed {:?}", wf_res)
    }

    env
}

// Add spec to env
// New fun if not exists
// Error if spec already exists for fun
fn add_spec(m: &mut HashMap<FunName, FunEnv>, v: &Lit) {
    // Find function name via the deep nested spec representation

    // TODO: A nicer way to unwrap?
    let Lit::Cons(val_const) = v else { todo!() };
    let outer_tuple = val_const.first().unwrap();
    let Lit::Tuple(spec_tuple) = outer_tuple else {
        todo!()
    };
    let fname_tuple = spec_tuple.first().unwrap();
    let Lit::Tuple(fname_list) = fname_tuple else {
        todo!()
    };
    let Lit::Atom(fun_name) = fname_list.first().unwrap() else {
        todo!()
    };
    let Lit::Int(arity) = &fname_list[1] else {
        todo!()
    };
    //println!("Got function name finally {:?} {:?}", fname, arity);
    if *arity < 0 {
        todo!("Handle negative arity.")
    }

    let fun_name = FunName {
        name: (*fun_name).clone(),
        arity: (*arity) as u64, // TODO: Does "as u64" convert as expected or just override the type definition?
    };

    // TODO: Potentially dangerous assumption about spec structure. Data may be lost here
    let spec_val = &spec_tuple[1..];

    let (fun_in, fun_out) = extract_spec(spec_val);

    // Lookup
    // TODO: A bit repetitive can repetition be avoided?
    match m.get(&fun_name) {
        Some(fun_env) => {
            // FunEnv exists, add spec if none
            if fun_env.spec.is_none() {
                m.insert(
                    fun_name,
                    FunEnv {
                        spec: Some((fun_in, fun_out)),
                        session: fun_env.session.clone(),
                        body: fun_env.body.clone(),
                    },
                );
            } else {
                panic!("Duplicate spec for {:?}", fun_name);
            }
        }
        None => {
            // Fresh entry, add new FunEnv
            m.insert(
                fun_name,
                FunEnv {
                    spec: Some((fun_in, fun_out)),
                    session: None,
                    body: None,
                },
            );
        }
    }

    // Add the "raw" spec value to map if possible or give error
}

// Add session to env
// New fun if not exists
// Error if session already exists for fun
fn add_session(m: &mut HashMap<FunName, FunEnv>, v: &Lit) {
    // Run session parser
    // Session type is wrapped within a list and then as the name of an atom
    // Get function name
    // Add Session type to map if possible or give error

    // TODO: A nicer way to unwrap?
    let Lit::Cons(val_const) = v else { todo!() };
    //println!("got that odd {:?}", v);

    // ASCII Decimal to string conversion...
    let mut st_string: String = String::new();
    for item in val_const {
        let Lit::Int(char_to_decode) = item else {
            todo!()
        };
        let char_to_decode_bytes = char_to_decode.to_be_bytes();
        let char_to_decode_last = char_to_decode_bytes.last().unwrap();
        st_string.push_str(
            std::str::from_utf8(&[*char_to_decode_last]).expect("bad todo not asci string"),
        );
    }

    //println!("\n\n{:?}\n\n", st_parse(&*st_string));

    let session_type_parsed: SessionDef = match st_parse(&st_string).finish() {
        Ok((_, res)) => res,
        Err(e) => panic!("Nom could not parse session type\n\n{}", e),
    };

    // Lookup
    // TODO: Repetitive?
    match m.get(&session_type_parsed.name) {
        Some(fun_env) => {
            // FunEnv exists, add spec if none
            if fun_env.session.is_none() {
                m.insert(
                    session_type_parsed.name.clone(),
                    FunEnv {
                        spec: fun_env.spec.clone(),
                        session: Some(session_type_parsed),
                        body: fun_env.body.clone(),
                    },
                );
            } else {
                panic!("Duplicate session for {:?}", session_type_parsed.name);
            }
        }
        None => {
            // Fresh entry, add new FunEnv
            m.insert(
                session_type_parsed.name.clone(),
                FunEnv {
                    spec: None,
                    session: Some(session_type_parsed),
                    body: None,
                },
            );
        }
    }
}

fn add_body(m: &mut HashMap<FunName, FunEnv>, fun_head: &FunName, fun_body: &FunDef) {
    match m.get(fun_head) {
        Some(fun_env) => {
            // FunEnv exists, add spec if none
            if fun_env.body.is_none() {
                m.insert(
                    fun_head.clone(),
                    FunEnv {
                        spec: fun_env.spec.clone(),
                        session: fun_env.session.clone(),
                        body: Some(fun_body.clone()),
                    },
                );
            } else {
                panic!("Duplicate body for {:?}", fun_head);
            }
        }
        None => {
            // Fresh entry, add new FunEnv
            m.insert(
                fun_head.clone(),
                FunEnv {
                    spec: None,
                    session: None,
                    body: Some(fun_body.clone()),
                },
            );
        }
    }
}

fn extract_spec(spec_in: &[Lit]) -> (Vec<Types>, Types) {
    // Convert tagged tuples into something that can be used to compare types

    // TODO: Is it possible to explain this function at all?
    // Erlang Abstract format it terrible to navigate...

    // It is best to panic if the format does not match (for now)
    // Implement as needed strategy

    // We expect the format to be something like
    // type fun
    // product - input types or single input type
    // product - output types or single output type

    // Location markers are removed (left over from the erlang abstract format)
    let outer_unwrap = spec_in.first().unwrap();
    let Lit::Cons(outer_list) = outer_unwrap else {
        todo!()
    };
    let Lit::Tuple(outer_tuple) = outer_list.first().unwrap() else {
        todo!()
    };

    // Make the tuple mutable by clone
    let mut outer_tuple = outer_tuple.clone(); // TODO: Seems a bit dirty...
    outer_tuple.drain(0..3); // TODO: Instead of mut copy + drain: Try ".get(4)" directly [4] did not work, but that may be fine?
    let Lit::Cons(main_spec_io_cons) = outer_tuple.first().unwrap() else {
        todo!()
    };
    //let mut main_spec_io_cons = main_spec_io_cons.clone();

    let main_spec_in = main_spec_io_cons.get(0).unwrap();
    let main_spec_out = main_spec_io_cons.get(1).unwrap();

    // Focus on input type extraction
    let Lit::Tuple(mut main_spec_in) = main_spec_in.clone() else {
        todo!()
    };
    main_spec_in.drain(0..3);
    let Lit::Cons(main_spec_in) = main_spec_in.first().unwrap() else {
        todo!()
    };

    let mut res_in_types: Vec<Types> = vec![];
    for in_spec_type in main_spec_in {
        let Lit::Tuple(mut in_spec_type) = in_spec_type.clone() else {
            todo!()
        };
        in_spec_type.drain(0..2);
        let Lit::Atom(Atom(type_string)) = in_spec_type.first().unwrap() else {
            todo!()
        };
        res_in_types.push(Types::Single(type_string.clone()));
    }

    // Focus on output type extraction
    let Lit::Tuple(mut main_spec_out) = main_spec_out.clone() else {
        todo!()
    };
    main_spec_out.drain(0..2);
    // TODO: Also support other types of types
    let Lit::Atom(Atom(out_type_string)) = main_spec_out.first().unwrap() else {
        todo!()
    };
    //println!("\n\n\nReady\nIn:{:?}\nOut:{:?}\n\n\n", main_spec_in, main_spec_out);

    (res_in_types, Types::Single(out_type_string.clone()))
}

// Add body to env
// New fun if not exists (Maybe it does not make sense to add functions if they have no session?)
// Error if function already exists for fun
