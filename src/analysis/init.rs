// Check ST annotation matches function body

use std::collections::HashMap;

use nom::Finish;

use crate::{
    cerl_parser::ast::{Atom, FunName, Lit, Module},
    st_parser::{
        ast::{ST, SessionDef, SessionType, Types, ST},
        parser::st_parse,
    },
};

use super::{
    env::{FunContract, FunEnv, Funcs},
    wellformed::check_wf,
};

// TODO: Use new FunContract instead to initialize all functions
// Extract relevant parts of the core erlang module for analysis
// Returns Funcs env and a list of functions that cannot be analyzed (due to lack of -spec and -session).
pub fn init_funcs_env(m: Module) -> (Funcs,Vec<FunName>) {
    // Info: Name of module and module exports not relevant yet
    let mut env: HashMap<FunName, FunEnv> = HashMap::new();

    let mut spec_args: HashMap<FunName, (Vec<FunContract>, Types)>;
    let mut session_args: HashMap<FunName, Vec<ST>>;

    let mut skipped_functions: Vec<FunName>;

    // Use attributes to get spec and session
    for attribute in &m.attributes {
        let Atom(a_name) = &attribute.name;
        if a_name.eq("spec") {
            add_spec(&mut spec_args, &attribute.value)
        }
        if a_name.eq("session") {
            add_session(&mut session_args, &attribute.value)
        }
        // TODO: Add support for custom type declarations?
    }

    // Use body to get body of functions
    // TODO: Adding functions to env is not optional here
    for (fun_head, fun_body) in &m.body {
        // Check whether function is qualified for analysis
        if !spec_args.contains_key(fun_head) || !session_args.contains_key(fun_head) {
            // Mark function as skipped
            skipped_functions.push(fun_head.clone());

            // Allow functions that are explicitly skipped to have unused -session and -spec
            // In other works: By removing them from the maps they are marked as "processed".
            spec_args.remove(fun_head);
            session_args.remove(fun_head);

            // Go to next function.
            continue;
        }

        if env.contains_key(&fun_head) {
            panic!("Duplicate body for function {}", fun_head)
        }
        
        // Get -spec and related return type
        let (fun_spec_in, fun_spec_rt) = spec_args.get(fun_head).unwrap();

        // Get -session and related return type
        let fun_session_in = session_args.get(fun_head).unwrap();

        // Note:     Session types are not native to erlang. They only exists during analysis
        //           Returned "valued" are references used as session identifiers.

        // Zip/Merge -spec and -session. Remember -session only supplements -spec
        // -spec is not replaced to preserve compatibility with Dialyzer etc.
        let mut fun_contract: Vec<FunContract>;
        for (i,arg_in) in fun_session_in.iter().enumerate() {
            match arg_in {
                SessionType::NotST => {
                    // Argument must be in -spec
                    fun_contract.push(fun_spec_in.get(i).unwrap().clone());
                },
                SessionType::New(st_new) => {
                    // Argument in -spec must be 'new' atom
                    if *fun_spec_in.get(i).unwrap() != FunContract::Base(Types::Single("new".to_owned())) {
                        todo!("Nice error message when new() is not used in -spec but new session type exists in -session")
                    }
                    fun_contract.push(FunContract::New(st_new.clone()));
                },
                SessionType::Ongoing(st_ongoing) => {
                    // Argument in -spec must be 'new' atom
                    if *fun_spec_in.get(i).unwrap() != FunContract::Base(Types::Single("ongoing".to_owned())) {
                        todo!("Nice error message when new() is not used in -spec but new session type exists in -session")
                    }
                    fun_contract.push(FunContract::Ongoing(st_ongoing.clone()));
                },
            }
        }

        // Add info to environment
        env.insert(
            fun_head.clone(),
            FunEnv {
                contract: fun_contract,
                return_type: fun_spec_rt.clone(), // TODO: Involved default, maybe just input the return type right away instead?
                body: Some(fun_body.clone()),
            },
        );

        // Remove used functions. The maps are checked in the end and should be empty.
        spec_args.remove(fun_head);
        session_args.remove(fun_head);
    }

    // For all functions: Decode "spec" to something that is consumable.
    // Maybe better to "just" decode them "at once"?
    // How to deal with split spec definitions? Multi-case spec

    // TODO: Temp contract/arguments type collection map to a vector. Ensure the list is wellformed.

    // TODO: Merge -spec and -session

    // TODO: Check "must_analyze":
    // if fun_env.spec.is_some() && fun_env.session.is_some() && fun_env.body.is_some() {
    // If not add "comment".

    // Sanity check: There should be no extra -spec or -session.
    if spec_args.len() > 0 || session_args.len() > 0 {
        todo!("Pretty error for unreachable/unusable -session and/or -spec definitions; I.e. they are defined for functions that are not defined.")
    }

    // Finally check data is well formed
    let wf_res = check_wf(m, &env);
    if wf_res.is_err() {
        panic!("Well-formed check failed {:?}", wf_res)
    }

    (env,vec![])
}

// Add spec to env
// New fun if not exists
// Error if spec already exists for fun
fn add_spec(args: &mut HashMap<FunName, (Vec<FunContract>, Types)>, v: &Lit) {
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
        todo!("Unexpected negative arity.")
    }

    let fun_name = FunName {
        name: (*fun_name).clone(),
        arity: (*arity) as u64, // TODO: Does "as u64" convert as expected or just override the type definition?
    };

    if args.contains_key(&fun_name) {
        panic!("Duplicate -spec for function {}", fun_name)
    }

    // TODO: Potentially dangerous assumption about spec structure. Data may be lost here
    let spec_val = &spec_tuple[1..];

    // Get the spec in vector format
    let (args_in,args_out) = extract_spec(spec_val);

    // Fresh entry, add new FunEnv
    args.insert(fun_name, (args_in,args_out));
}

// Add session to env
// New fun if not exists
// Error if session already exists for fun
fn add_session(args: &mut HashMap<FunName, Vec<ST>>, v: &Lit) {
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

    if args.contains_key(&session_type_parsed.name) {
        panic!("Duplicate -session for function {}", session_type_parsed.name)
    }

            args.insert(
                session_type_parsed.name.clone(),
                session_type_parsed.st,
            );
}

fn extract_spec(spec_in: &[Lit]) -> (Vec<FunContract>, Types) {
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
    let res_in_types = res_in_types.into_iter().map(FunContract::Base).collect();

    (res_in_types, Types::Single(out_type_string.clone()))
}

// Add body to env
// New fun if not exists (Maybe it does not make sense to add functions if they have no session?)
// Error if function already exists for fun
