use std::collections::{HashMap, HashSet};

use crate::{
    cerl_parser::ast::{Atom, Lit, Pat, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CType},
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
    let gsp_new = CFunCall::Call(Atom("gen_server_plus".to_owned()), Atom("new".to_owned()));
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
    // println!("TODO Call by value isolation!!!!! Important!!!");
    let CType::New(session_type) = (match expr(module, &mut TypeEnvs(envs.0.clone()), server_pid) {
        Ok(ok_val) => ok_val,
        Err(err_val) => return Err(format!("E_call gsp_new failed due to {}", err_val)),
    }) else {
        return Err("Must construct new session here".to_string());
    };
    // println!("type of pid {:?}", session_type);

    Ok(CType::Consume(None, session_type))

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
    let gsp_sync_send = CFunCall::Call(Atom("gen_server_plus".to_owned()), Atom("call".to_owned()));

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

    println!("First and second argument ar not checked right now. Should they?");

    // Get the third argument. This is the important value, can it be sent?
    let sending_expr = &args[2];
    // TODO Call by value isolation!!!!! Important!!!
    println!("TODO Call by value isolation!!!!! Important!!!");
    let CType::Base(sending_val) = (match expr(module, &mut TypeEnvs(envs.0.clone()), sending_expr)
    {
        Ok(ok_val) => ok_val,
        Err(err_val) => return Err(format!("e_call gsp_sync_send failed because {}", err_val)),
    }) else {
        return Err("e_call gsp_sync_send can only send base values".to_string());
    };
    //println!("{:?}", envs);

    // Get current session
    let session_id = &args[1];
    let CExpr::Var(session_var) = session_id else {
        return Err(
            "e_call gsp_sync_send Session variable name must be used, not expression".to_string(),
        );
    };
    let CType::Consume(_, session_type) =
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
                return Err("Expects ping-pong send-receive".to_string());
            };
            envs.0
                .insert(session_var.clone(), TypeEnv::Delta(session_type));
            Ok(CType::Base(received))
        }
        SessionType::Receive(_) => {
            Err("Session type says receive, we are about to send".to_string())
        }
        SessionType::MakeChoice(_, _) => {
            Err("Session type MakeChoice, expected OfferChoice".to_string())
        }
        SessionType::OfferChoice(offers) => {
            // Make choice
            let BaseType::Atom(Atom(atom_label)) = sending_val else {
                return Err(format!("Cannot make a choice without a label. Session type expects a choice {:?} {:?} {:?}", session_type, sending_val, args));
            };
            let try_label = Label(atom_label);
            match offers.get(&try_label) {
                Some(continuation) => {
                    // println!("DEBUG WAS HERE");
                    // TODO: Update ENV, gotta get session_var from argument.
                    envs.0
                        .insert(session_var.clone(), TypeEnv::Delta(continuation.clone()));
                    // print!("Updated Envs {:?} {:?}", session_var.clone(), envs);
                    Ok(CType::Consume(
                        Some(session_var.clone()),
                        continuation.clone(),
                    ))
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

pub fn e_case(
    module: &CModule,
    envs: &TypeEnvs,
    base_expr: &CExpr,
    clauses: &Vec<CClause>,
) -> Result<CType, String> {
    // Copy the env to save as a base case, we do not want to change the source env in this rule. The unchanged env must be returned.
    // Maybe a better way to types env clone?
    let case_start_envs = TypeEnvs(envs.0.clone());

    // Base expr type check
    // TODO: A bit manual env clone, maybe fix?
    let mut case_base_expr_envs = TypeEnvs(envs.0.clone());
    let base_res = expr(module, &mut case_base_expr_envs, base_expr);
    if base_res.is_err() {
        return Err(format!(
            "E_case failed in base case because {}",
            base_res.err().unwrap()
        ));
    }

    // base expr must return a consume session type, otherwise the choices cannot be checked against a session type
    // In other words: Any base or new type is a type error
    let CType::Consume(var, to_consume) = base_res.unwrap() else {
        return Err("Type error case base expr must be consume".to_string());
    };

    // Var must be defined
    let Some(var) = var else {
        return Err("Cannot work without binding session".to_string());
    };

    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    if clauses.is_empty() {
        // TODO: Actually remember. I decided that no clauses == Do
        //       Tbh this seemed like a simplification, but properly isn't, include Do in Contract Core Erlang?
        return Err("Zero clauses in case makes no sense.".to_string());
    }

    // TODO: If clauses.len() == 1 allow a bit more, do not require match on session.
    //       ^^ is the same as let x = e1 in e2
    //       Tbh this seemed like a simplification, but properly isn't, include Let in Contract Core Erlang?

    // Pattern for each case must match the current session type labels.
    // We require only that the clauses available must be in the session type, not complete session type coverage (TODO TODO TODO is this sane?)
    for clause in clauses {
        // Find match in current session type
        // TODO: Should we keep track of which parts of the session that has been reached/used ?
        let matching_st = lookup_st_from_label(&clause.pats, &to_consume);
        if matching_st.is_err() {
            return Err(format!(
                "Case clause label mismatch because {}",
                matching_st.err().unwrap()
            ));
        }

        let mut clause_envs = TypeEnvs(case_base_expr_envs.0.clone());
        clause_envs
            .0
            .insert(var.clone(), TypeEnv::Delta(matching_st.unwrap()));
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

    // Remember common return type for each clause, this common type is the return value.

    todo!()
}

// Check that all sessions are finished
pub fn finished(envs: &TypeEnvs) -> Result<(), String> {
    let empty: TypeEnvs = TypeEnvs(HashMap::new());
    diff_consumed(&empty, envs)
}

pub fn diff_consumed(before_envs: &TypeEnvs, after_envs: &TypeEnvs) -> Result<(), String> {
    // println!("\ndiff consume check.\nBefore env: {:?}", before_envs);
    // println!("After envs: {:?}", after_envs);

    // Check all that should be consumed, has been consumed (aka "finished" function)
    // Two stage: Find all to check, and then to check their value.
    let before_vars = before_envs.0.keys().cloned().collect();
    let after_vars: HashSet<Var> = after_envs.0.keys().cloned().collect();
    let must_be_consumed: HashSet<&Var> = after_vars.difference(&before_vars).collect();
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
    // println!("\nmust expr {:?}", e);
    // println!("before  {:?}", before_envs);
    // println!("current {:?}\n", current_envs);

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
    let new_vars: HashSet<Var> = new_envs.0.keys().cloned().collect();
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

fn lookup_st_from_label(
    pat: &[Pat],
    session_offers: &SessionTypesList,
) -> Result<SessionTypesList, String> {
    // We expect at very specific structure here:
    // The pattern must be one element long with just a atom
    // The session type must be a offer-type and one of the labels must be the pattern atom
    // We assume that the caller has checked the session_types list
    // If any of the above does not hold, we have a type error

    // Get the label for current pattern
    if pat.len() != 1 {
        return Err("Label cannot exist if length is not 1".to_string());
    }
    let Pat::Lit(atom) = pat.first().unwrap() else {
        return Err("label must be an atom #1".to_string());
    };
    let Lit::Atom(atom) = atom else {
        return Err("label must be an atom #2".to_string());
    };
    let crate::cerl_parser::ast::Atom(pat_label) = atom;

    // Look for label in session type
    if session_offers.0.len() != 1 {
        return Err("Length of session offers is not as expected".to_string());
    }
    let SessionType::OfferChoice(session_offers) = session_offers.0.first().unwrap() else {
        return Err("Case not possible without session offer".to_string());
    };

    // TODO: Can clone be avoided here?
    match session_offers.get(&Label(pat_label.clone())) {
        Some(st) => Ok(st.clone()),
        None => Err("No matching offer found in session!".to_string()),
    }
}
