use std::collections::{HashMap, HashSet};

use crate::{
    cerl_parser::ast::LitInner,
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CPat, CType},
        types::{BaseType, Label, SessionType, SessionTypesList},
    },
    type_checker::base::expr,
};

use super::env::{TypeEnv, TypeEnvs};

// Gen Server Plus Establish New Session
pub fn gsp_new(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    let gsp_new = CFunCall::Call("gen_server_plus".to_owned(), "new".to_owned());
    if *call != gsp_new {
        return Err("Not gen server plus new session constructor".to_string());
    }

    // To find the right sub-call we need to check the args. There must be three args
    if args.len() != 1 {
        return Err(format!(
            "gen_server_plus:new only works with one argument. {:?}",
            args
        ));
    }
    // Get the third argument. This is the important value
    let server_pid = args.first().unwrap();

    // TODO Call by value isolation!!!!! Important!!!
    let CType::New(session_type) = (match expr(module, &mut TypeEnvs(envs.0.clone()), server_pid) {
        Ok(ok_val) => ok_val,
        Err(err_val) => return Err(format!("E_call gsp_new failed due to {}", err_val)),
    }) else {
        return Err("Must construct new session here".to_string());
    };

    Ok(CType::Consume(session_type))

    // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
    // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
    // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
    // Generic "check env before-after" is strongly needed now!

    // We can only send base values

    // If it is not a base-value we assume it is select
}

// Gen Server Plus Sync Send
pub fn gsp_sync_send(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    let gsp_sync_send = CFunCall::Call("gen_server_plus".to_owned(), "call".to_owned());

    if *call != gsp_sync_send {
        return Err("Not Gen Server Plus Call".to_string());
    }
    // To find the right sub-call we need to check the args. There must be three args
    if args.len() != 3 {
        return Err(format!(
            "gen_server_plus:call only works with three arguments. {:?}",
            args
        ));
    }

    println!("TODO: First and second argument are not checked right now. Should they?");

    // Get the third argument. This is the important value, can it be sent?
    let sending_expr = &args[2];
    let CType::Base(sending_val) = (match expr(module, &mut TypeEnvs(envs.0.clone()), sending_expr)
    {
        Ok(ok_val) => ok_val,
        Err(err_val) => return Err(format!("e_call gsp_sync_send failed because {}", err_val)),
    }) else {
        return Err("e_call gsp_sync_send can only send base values".to_string());
    };

    // Get current session
    let session_id = &args[1];
    let CExpr::Var(session_var) = session_id else {
        return Err(
            "e_call gsp_sync_send Session variable name must be used, not expression".to_string(),
        );
    };
    let CType::Consume(session_type) =
        (match expr(module, &mut TypeEnvs(envs.0.clone()), session_id) {
            Ok(ok_val) => ok_val,
            Err(err_val) => {
                return Err(format!(
                    "e_call gsp_sync_send failed because of {}",
                    err_val
                ))
            }
        })
    else {
        return Err("e_call gsp_sync_send second argument must be session type".to_string());
    };
    let mut session_type = session_type;

    // TODO: session_var must match, if not defined, set it!

    // Session must have at least one element otherwise it is not possible to continue:
    if session_type.0.is_empty() {
        return Err("e_call gsp_sync_send Cannot send on empty/consumed session".to_string());
    }

    // It can be either an atom matching a label, or a simple value sent. Let us check:
    match session_type.0.first().unwrap() {
        SessionType::Send(to_send_val) => {
            if session_type.0.len() < 2 {
                return Err("Session type too short for sync send-receive".to_string());
            }
            // Send base value
            if sending_val != *to_send_val {
                return Err("Mismatch between expected to send and actual type.".to_string());
            }
            session_type.0.remove(0);
            // Return type is received value
            let SessionType::Receive(received) = session_type.0.remove(0) else {
                // TODO: Do not return error here. This flow is normal and expected!
                // Make choice should be fine. The time this type is used a choice must be made.
                // Subtype relation should work here.
                return Err("Expects ping-pong send-receive".to_string());
            };
            envs.0
                .insert(session_var.clone(), TypeEnv::Delta(session_type));
            Ok(CType::Base(received))
        }
        SessionType::Receive(_) => {
            Err("Session type says receive, we are about to send".to_string())
        }
        SessionType::MakeChoice(_) => {
            Err("Session type MakeChoice, expected OfferChoice".to_string())
        }
        SessionType::OfferChoice(offers) => {
            // Make choice
            let BaseType::Atom(atom_label) = sending_val else {
                return Err(format!("Cannot make a choice without a label. Session type expects a choice {:?} {:?} {:?}", session_type, sending_val, args));
            };
            let try_label = Label(atom_label);
            match offers.get(&try_label) {
                Some(continuation) => {
                    // TODO: Update ENV, gotta get session_var from argument.
                    envs.0
                        .insert(session_var.clone(), TypeEnv::Delta(continuation.clone()));
                    Ok(CType::Consume(continuation.clone()))
                }
                None => Err("Trying to make choice not offered by session".to_string()),
            }
        }
        SessionType::End => Err("Session type is End, but we are about to use it".to_string()),
    }

    // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
    // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
    // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
    // Generic "check env before-after" is strongly needed now!

    // We can only send base values

    // If it is not a base-value we assume it is select
}

// Session type branching
pub fn e_case_offer(
    module: &CModule,
    envs: &TypeEnvs,
    offers: &HashMap<Label, SessionTypesList>,
    clauses: &Vec<CClause>,
) -> Result<CType, String> {
    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    let mut case_start_envs = TypeEnvs(envs.0.clone());

    // Pattern for each case must match the current session type labels.
    // We require only that the clauses available must be in the session type, not complete session type coverage (TODO TODO TODO is this sane?)
    for clause in clauses {
        // Find match in current session type
        let mut clause_envs = TypeEnvs(envs.0.clone());

        // Get the label for current patten
        let [CPat::Lit(label)] = clause.pats.as_slice() else {
            return Err("label must be an atom #1".to_string());
        };
        let LitInner::Atom(label) = label else {
            return Err("label must be an atom #2".to_string());
        };

        // Look for label in session type
        // TODO: Can clone be avoided here?
        let Some(matching_st) = offers.get(&Label(label.clone())) else {
            return Err("No matching offer found in session!".to_string());
        };

        // TODO: It looks like I am missing something. Maybe base_expr var name? Or am I odd to
        // think so?

        // TODO: Potential optimization: Before vars are computed for each clause, where they could be computed outside loop.
        let clause_res =
            must_st_consume_expr(module, &case_start_envs, &mut clause_envs, &clause.res);
        if clause_res.is_err() {
            return Err(format!(
                "Case clause failed because {}",
                clause_res.err().unwrap()
            ));
        }

        // Add return type to check later
        // TODO: Check common_return_type in each iteration instead of later.
        common_return_type.push(clause_res.clone().unwrap());
    }

    // Check return-type is the same.
    let common_return_type_base = common_return_type.first().unwrap();
    common_return_type
        .iter()
        .all(|item| *item == *common_return_type_base);

    Ok(common_return_type_base.clone())
}

// Check that all sessions are finished
pub fn finished(envs: &TypeEnvs) -> Result<(), String> {
    let empty: TypeEnvs = TypeEnvs(HashMap::new());
    diff_consumed(&empty, envs)
}

pub fn diff_consumed(before_envs: &TypeEnvs, after_envs: &TypeEnvs) -> Result<(), String> {
    // Check all that should be consumed, has been consumed (aka "finished" function)
    // Aka: Check all newly defined variables are consumed (if required).
    // Two stage: Find all to check, and then to check their value.
    let before_vars = before_envs.0.keys().cloned().collect();
    let after_vars: HashSet<String> = after_envs.0.keys().cloned().collect();
    // must_be_consumed == new variables that only exist within the enclosed let in
    let must_be_consumed: HashSet<&String> = after_vars.difference(&before_vars).collect();
    for check_var in must_be_consumed {
        match after_envs.0.get(check_var).unwrap() {
            TypeEnv::Gamma(_) => continue,
            TypeEnv::Delta(check_consumed) => {
                // Check session is consumed
                if check_consumed.0.is_empty() {
                    continue;
                }
                if check_consumed.0.len() == 1 {
                    if *check_consumed.0.first().unwrap() == SessionType::End {
                        continue;
                    }
                    return Err(format!("{:?} Not consumed {:?}", check_var, check_consumed));
                }
                return Err(format!("{:?} Not consumed {:?}", check_var, check_consumed));
            }
            TypeEnv::Sigma(_) => continue,
        }
    }
    Ok(())
}

pub fn must_st_consume_expr(
    module: &CModule,
    before_envs: &TypeEnvs,
    current_envs: &mut TypeEnvs,
    e: &CExpr,
) -> Result<CType, String> {
    // Check e2 in double mark
    match expr(module, current_envs, e) {
        Ok(return_type) => {
            // Check finished for all new sessions, diff between environments
            let diff_ok = diff_consumed(before_envs, current_envs);
            if let Err(err_val) = diff_ok {
                return Err(format!(
                    "must st consume expr failed because {} \nWhile checking {:?} \n",
                    err_val, e
                ));
            }

            // Env isolation step: Remove all new definitions, unless they are session identifiers
            // In this case, it would have been easier to have the envs separate
            envs_isolation(before_envs, current_envs);

            // Return return_type
            Ok(return_type)
        }
        Err(err_val) => Err(format!("must_st_consume_expr failed because {}", err_val)),
    }
}

// Task: Ensure that we preserve isolation of all environments but except Delta. Remove new values and restore old values.
// Used to e.g. ensure arguments are evaluated in an isolated environment.
fn envs_isolation(old_envs: &TypeEnvs, new_envs: &mut TypeEnvs) {
    let new_vars: HashSet<String> = new_envs.0.keys().cloned().collect();
    // Check current env. For all that is not Delta, the definition must be the same as in before. Remove or update as needed
    for var_key in new_vars {
        let cur_val = new_envs.0.get(&var_key).unwrap();
        match cur_val {
            TypeEnv::Delta(_) => continue, // Keep only changes in Delta
            _ => match old_envs.0.get(&var_key) {
                Some(old_val) => {
                    new_envs.0.insert(var_key.clone(), old_val.clone());
                }
                None => {
                    new_envs.0.remove(&var_key);
                }
            },
        }
    }
}
