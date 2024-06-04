use std::collections::{HashMap, HashSet};

use crate::{
    cerl_parser::ast::{Lit, Pat, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CType},
        types::{Label, SessionType, SessionTypesList},
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
    todo!()
}

// Gen Server Plus Sync Send
pub fn gsp_sync_send(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    todo!()
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
            if diff_ok.is_err() {
                return  Err(format!("must st consume expr {:?} failed because {}\n DEBUG:\nBefore Envs: {:?}\nAfter Envs: {:?}", e, diff_ok.err().unwrap(), before_envs, current_envs));
            }

            // Return return_type
            Ok(return_type)
        }
        Err(err_val) => Err(format!("must_st_consume_expr failed because {}", err_val)),
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
