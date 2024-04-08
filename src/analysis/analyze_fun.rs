// We need to know what functions returns

// There may be user defined functions in which case we take the -spec for that function
// For any build in function we must write a function that can return the correct type

// TODO: What about functions with more than one possible return type? ... Hmm
// Ignored for now...

// The function information environment is not updated while checking the session-types.
// This environment is used for lookup when variables are assigned or session-types checked.

use std::collections::{HashMap, HashSet};

use crate::{
    analysis::analyze_expr::env_update_pattern_from_return_type,
    cerl_parser::ast::{Atom, Expr, Exprs, FunCall, FunKind, FunName, Pat, Var},
    st_parser::ast::{SessionElementList, SessionType, Types},
};

use super::{
    analyze_st::extract_var_type,
    env::{FunEnv, VarType},
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
        return Err("Only call supported so far bif check".to_string());
    };
    // TODO: Does as_str imply a clone ?
    match (kind.as_str(), name.as_str()) {
        ("io", "format") => Ok(Types::Single("no_return".to_owned())),
        _ => Err(format!("no bif match on {:?}", call)),
    }
}

pub fn get_user_fun_type(
    m: &HashMap<FunName, FunEnv>,
    env: &HashMap<Var, VarType>,
    call: &FunCall,
    args: &Vec<Exprs>,
) -> Result<Vec<(VarType, HashMap<Var, VarType>)>, String> {
    // Steps
    // 0) Get fun name
    let FunCall {
        kind: kind_type,
        name,
    } = call;
    if *kind_type != FunKind::Apply {
        return Err(format!(
            "Currently only apply is supported here. Got {:?} {:?}",
            call, args
        ));
    }

    // FunName has to be reconstructed as arity info is currently thrown away when parsing after a sanity check: TODO Reconsider this approach
    let fun_name = FunName {
        name: name.clone(),
        arity: args.len().try_into().unwrap(),
    };

    // 1) Is function defined at all? - check m

    let fun_env: &FunEnv = match m.get(&fun_name) {
        Some(res) => res,
        None => return Err(format!("Function {:?} was not found", fun_name)),
    };

    let mut cur_env = env.clone();

    //let mut possible_envs: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
    let mut seen_arg_names: HashSet<Var> = HashSet::new();

    // Get type from fun_env, maybe based on rt ?
    let (input_spec, output_spec) = fun_env.spec.as_ref().unwrap();

    // 3) Match argument types with function header - use looked up value in m
    // 2) Are the arguments defined, if so get the types of those - get vars from args check in env
    for (i, elm) in args.iter().enumerate() {
        // All arguments are expressions so their value might not be immediately known,
        // gotta call back to the general type checker for exprs
        // This might also give more than one possible execution.
        // For each of the possible environments that do not lead to an error check the var type is acceptable
        // println!("DEBUG Number checking is {:?}", i);
        // Note: TODO Only support variable-type arguments to functions to avoid "how to update env" issues.
        //            Solution would be to use chk_st_exprs and check all possible outcomes there.
        let Exprs(var) = elm;
        if var.len() != 1 {
            return Err(
                "Does only support st where arg is exprs of length one currently".to_string(),
            );
        }
        let Expr::Var(var) = var.first().unwrap() else {
            return Err(
                "Analyzer currently required all arguments to be variable names.".to_string(),
            );
        };
        if seen_arg_names.contains(var) {
            return Err("Cannot handle duplicate argument names currently.".to_string());
        }
        seen_arg_names.insert(var.clone());

        let spec_session = extract_var_type(
            fun_env.session.as_ref().unwrap().st.get(i).unwrap(),
            input_spec.get(i).unwrap(),
        );
        if !env.contains_key(var) {
            return Err("Undefined variable used. Not supported.".to_string());
        }
        let env_rt = env.get(var).unwrap().clone();
        // println!(
        //     "DEBUG got return type via exprs analysis: {:?} and the spec says the type is: {:?}",
        //     env_rt, spec_session
        // );
        // if spec_session_rt is ST(Ongoing( ... , ... )) then try to consume part of rt and check leftover matches
        if let VarType::ST(SessionType::Ongoing(spec_st_in, spec_st_out)) = spec_session {
            // Try to consume session type
            let VarType::ST(SessionType::Ongoing(env_rt_in, _env_rt_out)) = env_rt else {
                return Err(format!(
                    "Mismatch ongoing types. Expected {} to be ongoing to match {} in spec.",
                    env_rt, spec_st_in
                ));
            };
            let SessionElementList(spec_st_in) = spec_st_in;
            let SessionElementList(mut env_rt_in) = env_rt_in;
            for spec_st_in_elm in spec_st_in {
                if let Some(cur_env_compare) = env_rt_in.first() {
                    if *cur_env_compare != spec_st_in_elm {
                        return Err(format!(
                            "In function call when trying to match {} in spec we found {} in env.",
                            spec_st_in_elm, cur_env_compare
                        ));
                    }
                    env_rt_in.drain(..1);
                } else {
                    return Err(format!(
                        "In function call when trying to match {} in spec we found Nothing in env.",
                        spec_st_in_elm
                    ));
                }
            }
            // output spec is used for the return type aka possible env
            if spec_st_out.is_none() {
                return Err("Session spec should have a return type.".to_string());
            }
            // TODO: I need the variable name if the env_rt is a session type.
            //       Maybe it is OK only to support via var name direct? E.g. _6 and then update env that way?
            // Simulate variable update by constructing a let expressions for assignment
            // TODO: VERY IMPORTANT: This way of updating the analysis env only works when there is just one argument with ongoing!

            cur_env = env_update_pattern_from_return_type(
                cur_env.clone(),
                vec![Pat::Var(var.clone())],
                VarType::ST(SessionType::Ongoing(spec_st_out.unwrap(), None)),
            )
            .unwrap();

            // return  type for function is of course the function type itself
        } else {
            // Otherwise for sanity check the argument types should match
            if env_rt != spec_session {
                return Err(format!(
                    "Mismatch argument types in function call. Expected  {:?} but got {:?}",
                    spec_session, env_rt
                ));
            }
        }
    }

    // 3a) If ST: Update env

    // TODO: Maybe update return type to be singular or keep ready for multi exprs arguments?
    // TODO: Support ST retun types, right now only supporting base type returns.
    Ok(vec![(VarType::Base(output_spec.clone()), cur_env)])
}
