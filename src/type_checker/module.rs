use std::collections::HashMap;

use crate::{
    cerl_parser::ast::Atom,
    contract_cerl::{
        ast::{CModule, CType, OptWarnings},
        types::{BaseType, SessionType, SessionTypesList},
    },
};

use super::{base::expr, env::TypeEnvs, init::init_env, session::finished};

#[derive(Eq, PartialEq)]
struct MSpecEnv(HashMap<MSpec, bool>);

#[derive(Eq, Hash, PartialEq)]
struct MSpec {
    state_in: Atom,
    state_out: Atom,
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
    println!("mspec: {:?}", module.mspec);
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
        // TODO: Assumption: The label "__START__" is used internally
        mspec_handle_extractor(&mut handle_map, Atom("__START__".to_string()), mspec);

        //
        //
        // For all handle call matche with the input above
        //
        // Ensure that all parts of the session type has one and only one implementation. If there
        // are none or more than one implementation be sure to fail the module wrt to the mspec
        // type checking.
    }

    // each function get a fresh env
    for (fun_name, clauses) in &module.functions {
        // Check each function clause
        for clause in clauses {
            // Define envs hashmap for this clause
            let mut envs: TypeEnvs = TypeEnvs(HashMap::new());

            // Init env based on function header
            let init_ok = init_env(&mut envs, &clause.args, &clause.spec);
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
                overall_acceptance = false
            };

            // If something went wrong set overall_acceptance to false
        }
    }
    OptWarnings {
        res: overall_acceptance,
        warnings,
    }
}

fn mspec_handle_extractor(map: &mut MSpecEnv, state: Atom, input: &SessionTypesList) {
    // To extract I need a label and to include all until next label.
    // TODO: Fixed pattern assumed now: When receive then follow by send
    // When choise, follow by send "received"
    // it is always possible to end session in addition to sending Data
    // Next label should be given for checking
    // This system is a bit restrictive but at least a start.

    match input.0.as_slice() {
        // pattern match mspec extraction
        [SessionType::Receive(recv), SessionType::Send(send), SessionType::End] => {
            map.0.insert(
                MSpec {
                    state_in: state,
                    state_out: Atom("session_end".to_string()),
                    val_in: recv.clone(),
                    val_out: send.clone(),
                },
                false,
            );
        }
        _ => todo!("Not supported mspec. Failed in mspec handle extraction."),
    }
}
