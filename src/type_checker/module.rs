use std::{collections::HashMap, rc::Rc};

use crate::{
    cerl_parser::ast::{Atom, FunName, Var},
    contract_cerl::{
        ast::{CModule, CType, OptWarnings},
        types::{BaseType, ChoiceType, SessionType},
    },
};

use super::{base::expr, env::TypeEnvs, init::init_env, session::finished};

#[derive(Debug, Eq, PartialEq)]
struct MSpecEnv(HashMap<MSpec, bool>);

#[derive(Debug, Eq, Hash, PartialEq)]
struct MSpec {
    state_in: Rc<Atom>,
    state_out: Rc<Atom>,
    val_in: BaseType,
    val_out: BaseType,
}

// Entry point for type checking.
// One module in, no environment required
// Return acceptance or failure and a list of messages
pub fn module(module: CModule) -> OptWarnings<bool> {
    if module.functions.is_empty() {
        return OptWarnings {
            res: false,
            warnings: vec![format!("Nothing to analyze, not acceptable")],
        };
    }
    let mut warnings: Vec<String> = Vec::new();
    // Overall acceptance. Assume all is OK until proven otherwise
    let mut overall_acceptance = true;

    // TODO: Consistency sanity check: If mspec also expect behavior('gen_server_plus')
    // If mspec is defined check module mspec first
    if let Some(mspec) = &module.mspec {
        // Approach: Something ala: Generate input to run checker
        //
        // Split the mspec into parts, kinda like a code generator, except that it is only cares
        // about checking the -spec. Is that okay ?
        // TODO: Should mspec check the content of each handle call also? Or is it enough to just
        // check the spec and rely on e.g. dialyzer to ensure the function body is ok?
        //
        // Data could be a map: (State, Input, Output) => Seen (yes/no)
        // All must have been seen by end, and no duplicates.
        // Extras are accepted for now. Intention is to be less invasive TODO: Good or bad idea?
        //
        // Construct via recrsive function on the SessionTypesList
        let mut handle_map = MSpecEnv(HashMap::new());
        // TODO: Assumption: The label "start" is used internally
        mspec_handle_extractor(
            &mut handle_map,
            &mut HashMap::new(),
            &Atom("start".to_string()).into(),
            mspec.0.as_slice(),
        );

        //
        //
        // For all handle call match with the input above
        //
        // Ensure that all parts of the session type has one and only one implementation. If there
        // are none or more than one implementation be sure to fail the module wrt to the mspec
        // type checking.
        // Accept extra handle calls for now. TODO: Maybe this is a bad idea as they could
        //                                          interfere with the communication.

        if let Some(plus_calls) = module.functions.get(&FunName {
            name: Atom("handle_plus_call".to_string()).into(),
            arity: 5,
        }) {
            for plus_clause in plus_calls {
                let [val_in, _, state_in, _, _] = plus_clause.spec.as_slice() else {
                    continue;
                };
                // TODO: Here we assume base type for value in. Always true?
                let CType::Base(val_in) = val_in else {
                    continue;
                };
                let CType::Base(state_in) = state_in else {
                    continue;
                };
                let BaseType::Atom(state_in) = state_in else {
                    continue;
                };
                let BaseType::Tuple(return_type_tuple) = &plus_clause.return_type else {
                    continue;
                };
                let [handle_action, val_out, state_out, _, _global_state] =
                    return_type_tuple.as_slice()
                else {
                    continue;
                };
                if handle_action != &BaseType::Atom(Atom("reply".to_string()).into()) {
                    continue;
                }
                let BaseType::Atom(state_out) = state_out else {
                    continue;
                };
                let find_mspec = MSpec {
                    state_in: state_in.clone(),
                    state_out: state_out.clone(),
                    val_in: val_in.clone(),
                    val_out: val_out.clone(),
                };
                match handle_map.0.get(&find_mspec) {
                    Some(res) => {
                        if !res {
                            handle_map.0.insert(find_mspec, !res);
                        } else {
                            todo!("mspec failed, find proper err msg");
                        }
                    }
                    None => continue,
                };
            }
        }

        for (mspec, val) in handle_map.0 {
            if !val {
                warnings.push(format!(
                    "Type checking failed for module {} as the mspec requires {:?} which was not found.",
                    module.name,
                    mspec
                ));
                overall_acceptance = false;
            }
        }
    }

    // each function get a fresh env
    for (fun_name, clauses) in &module.functions {
        // Check each function clause
        for clause in clauses {
            // Define envs hashmap for this clause
            let mut envs: TypeEnvs = TypeEnvs(HashMap::new());

            let Some(fallback) = module.fallback_args.get(fun_name) else {
                todo!("No fallback argument found.");
            };

            // Init env based on function header
            let init_ok = init_env(&mut envs, &clause.args, &clause.spec, fallback);
            if init_ok.is_err() {
                warnings.push(format!(
                    "Init env failed for {} due to {}",
                    fun_name,
                    init_ok.err().unwrap()
                ));
                overall_acceptance = false;
                continue;
            }

            // Type check body, updates envs in place
            // module is sent along to give access to function signatures
            let return_type = expr(&module, &mut envs, &clause.body);
            if return_type.is_err() {
                warnings.push(format!(
                    "Type checking failed for {} due to {}",
                    fun_name,
                    return_type.err().unwrap()
                ));
                overall_acceptance = false;
                continue;
            }

            // Check return-type somehow?
            // TODO: Possible performance optimization:
            // Instead of clone, unpack return_type and compare directly
            let clause_actual_return_type = return_type.unwrap();
            if clause_actual_return_type != CType::Base(clause.return_type.clone()) {
                warnings.push(format!(
                    "Wrong return type for function {}. Expected {:?} but found {:?}",
                    fun_name, clause.return_type, clause_actual_return_type
                ));
                overall_acceptance = false;
            }

            // Check result, i.e. everything that should be consumed, has been consumed
            if let Err(err_val) = finished(&envs) {
                warnings.push(format!("Function contract for {} does not hold as all sessions are not fully consumed: {}", fun_name, err_val));
                // TODO: Find a way to deal with residual session types. Right now we accept not
                // fully consumed sessions at end of function. But we will give a warning.
                // overall_acceptance = false
            };

            // If something went wrong set overall_acceptance to false
        }
    }
    OptWarnings {
        res: overall_acceptance,
        warnings,
    }
}

fn mspec_handle_extractor(
    map: &mut MSpecEnv,
    rec_states: &mut HashMap<Rc<Var>, Rc<Atom>>,
    state_in: &Rc<Atom>,
    input: &[SessionType],
) {
    // To extract I need a label and to include all until next label.
    // TODO: Fixed pattern assumed now: When receive then follow by send
    // When choise, follow by send "received"
    // it is always possible to end session in addition to sending Data
    // Next label should be given for checking
    // This system is a bit restrictive but at least a start.

    match input {
        // pattern match mspec extraction
        [SessionType::Receive(recv), SessionType::Send(send), SessionType::End] => {
            map.0.insert(
                MSpec {
                    state_in: state_in.clone(),
                    state_out: Atom("session_end".to_string()).into(),
                    val_in: recv.clone(),
                    val_out: send.clone(),
                },
                false,
            );
        }
        [SessionType::Receive(recv), SessionType::Send(send), SessionType::State(state_out), remainder @ ..] =>
        {
            if remainder.is_empty() {
                todo!("Proper error for when state in mspec session type set right before end of session type")
            }
            map.0.insert(
                MSpec {
                    state_in: state_in.clone(),
                    state_out: state_out.clone(),
                    val_in: recv.clone(),
                    val_out: send.clone(),
                },
                false,
            );
            mspec_handle_extractor(map, rec_states, state_out, remainder)
        }
        [SessionType::Receive(recv), SessionType::Send(send), SessionType::Var(var)] => {
            map.0.insert(
                MSpec {
                    state_in: state_in.clone(),
                    state_out: rec_states.get(var).unwrap().clone(),
                    val_in: recv.clone(),
                    val_out: send.clone(),
                },
                false,
            );
        }
        [SessionType::Receive(recv), SessionType::State(state_out), remainder @ ..] => {
            if remainder.is_empty() {
                todo!("Proper error for when state in mspec session type set right before end of session type")
            }
            map.0.insert(
                MSpec {
                    state_in: state_in.clone(),
                    state_out: state_out.clone(),
                    val_in: recv.clone(),
                    val_out: BaseType::Atom(Atom("received".to_string()).into()), // TODO: Should implicit
                                                                                  // value here be explicit?
                },
                false,
            );
            mspec_handle_extractor(map, rec_states, state_out, remainder)
        }
        [SessionType::Choice(ct, offers)] if *ct == ChoiceType::Offer => {
            for (label, inner) in offers {
                // TODO: To offer label is converted into to receive label and send back received.
                // The residual session type is checked recursively.
                match inner.0.as_slice() {
                    [SessionType::State(state_out), remainder @ ..] if !remainder.is_empty() => {
                        map.0.insert(
                            MSpec {
                                state_in: state_in.clone(),
                                state_out: state_out.clone(),
                                val_in: BaseType::Atom(Atom(label.0.clone()).into()),
                                val_out: BaseType::Atom(Atom("received".to_string()).into()),
                            },
                            false,
                        );
                        mspec_handle_extractor(map, rec_states, state_out, remainder)
                    }
                    [SessionType::End] => {
                        map.0.insert(
                            MSpec {
                                state_in: state_in.clone(),
                                state_out: Atom("session_end".to_string()).into(),
                                val_in: BaseType::Atom(Atom(label.0.clone()).into()),
                                val_out: BaseType::Atom(Atom("received".to_string()).into()),
                            },
                            false,
                        );
                    }
                    _ => todo!("Could not check offer choice in mspec"),
                }
            }
        }
        [SessionType::Rec(var), remainder @ ..] => {
            if rec_states.contains_key(var) {
                todo!("Handle shadowing");
            }
            rec_states.insert(var.clone(), state_in.clone());
            mspec_handle_extractor(map, rec_states, state_in, remainder)
        }
        x => todo!(
            "Not supported mspec. Failed in mspec handle extraction: {:?}",
            x
        ),
    }
}
