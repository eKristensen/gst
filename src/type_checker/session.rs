use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    rc::Rc,
};

use crate::{
    cerl_parser::ast::{Atom, CLoc, Lit, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CModule, CPat, CType},
        types::{BaseType, ChoiceType, Label, SessionType, SessionTypesList},
    },
    type_checker::{base::expr, casting::add_gradual_cast, casting::get_cexpr_loc},
};

use super::env::{CastEnv, TypeEnv, TypeEnvs};

// Gen Server Plus Establish New Session
pub fn gsp_new(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    server_pid: &CExpr,
) -> Result<(Rc<CLoc>, CType), String> {
    // TODO Call by value isolation!!!!! Important!!!
    let CType::New(session_type) =
        (match expr(module, &mut TypeEnvs(envs.0.clone()), cast_env, server_pid) {
            Ok((_, ok_val)) => ok_val,
            Err(err_val) => return Err(format!("E_call gsp_new failed due to {}", err_val)),
        })
    else {
        return Err("Must construct new session here".to_string());
    };

    // let session_type = st_dual(&session_type)?;

    Ok((loc.clone(), CType::Consume(session_type)))

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
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    session_id: &CExpr,
    sending_expr: &CExpr,
) -> Result<(Rc<CLoc>, CType), String> {
    let CType::Base(sending_val) = (match expr(
        module,
        &mut TypeEnvs(envs.0.clone()),
        cast_env,
        sending_expr,
    ) {
        Ok((_, ok_val)) => ok_val,
        Err(err_val) => return Err(format!("e_call gsp_sync_send failed because {}", err_val)),
    }) else {
        return Err("e_call gsp_sync_send can only send base values".to_string());
    };

    // Get current session
    let CExpr::Var(_loc, session_var) = session_id else {
        return Err(
            "e_call gsp_sync_send Session variable name must be used, not expression".to_string(),
        );
    };
    let CType::Consume(session_type) =
        (match expr(module, &mut TypeEnvs(envs.0.clone()), cast_env, session_id) {
            Ok((_, ok_val)) => ok_val,
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

    let session_type = unfold(&session_type).0;
    let session_type = session_type.as_slice();

    match session_type {
        // Session must have at least one element otherwise it is not possible to continue:
        [] => Err("e_call gsp_sync_send Cannot send on empty/consumed session".to_string()),
        [SessionType::Send(to_send_val), SessionType::Receive(received), tail @ ..]
            if sending_val == *to_send_val || sending_val == BaseType::Dynamic =>
        {
            if sending_val == BaseType::Dynamic {
                println!("INFO: Requested cast to be inserted.");
                add_gradual_cast(
                    cast_env,
                    &get_cexpr_loc(sending_expr),
                    &CType::Base(BaseType::Dynamic),
                    &CType::Base(to_send_val.clone()),
                )?;
            }
            envs.0.insert(
                session_var.clone(),
                TypeEnv::Delta(SessionTypesList(tail.to_vec())),
            );
            Ok((loc.clone(), CType::Base(received.clone())))
        }
        [SessionType::Choice(ct, offers)] if *ct == ChoiceType::Make => {
            // Make choice
            let BaseType::Atom(atom_label) = sending_val else {
                return Err(format!(
                    "Cannot make a choice without a label. Session type expects a choice {:?} {:?}",
                    session_type, sending_val
                ));
            };
            let try_label = Label(atom_label.0.clone());
            match offers.get(&try_label) {
                Some(continuation) => {
                    // TODO: Update ENV, gotta get session_var from argument.
                    envs.0
                        .insert(session_var.clone(), TypeEnv::Delta(continuation.clone()));
                    Ok((loc.clone(), CType::Consume(continuation.clone())))
                }
                None => Err("Trying to make choice not offered by session".to_string()),
            }
        }
        _ => Err(format!(
            "Cannot perform next step {:?} in session with this state: {:?}",
            sending_val, session_type
        )),
    }

    // ----------------------------------------------------------------------------------------

    // TODO: session_var must match, if not defined, set it!

    // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
    // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
    // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
    // Generic "check env before-after" is strongly needed now!

    // We can only send base values

    // If it is not a base-value we assume it is select
}

// Gen Server Plus close session (end)
pub fn gsp_close(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    session_id: &CExpr,
) -> Result<(Rc<CLoc>, CType), String> {
    // Get current session
    let CExpr::Var(_loc, session_var) = session_id else {
        return Err(
            "e_call gsp_close Session variable name must be used, not expression".to_string(),
        );
    };
    let CType::Consume(session_type) =
        (match expr(module, &mut TypeEnvs(envs.0.clone()), cast_env, session_id) {
            Ok((_, ok_val)) => ok_val,
            Err(err_val) => return Err(format!("e_call gsp_close failed because of {}", err_val)),
        })
    else {
        return Err("e_call gsp_close second argument must be session type".to_string());
    };

    if let [SessionType::End] = session_type.0.as_slice() {
        envs.0.insert(
            session_var.clone(),
            TypeEnv::Delta(SessionTypesList(vec![])),
        );
        Ok((
            loc.clone(),
            CType::Base(BaseType::Atom(Atom("received".to_string()).into())),
        ))
    } else {
        Err(format!(
            "e_call gsp_close requires session to be ready to be closed. Found {:?}",
            session_type
        ))
    }
}

// Session type branching
pub fn e_case_offer(
    module: &CModule,
    envs: &TypeEnvs,
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    offers: &BTreeMap<Label, SessionTypesList>,
    clauses: &Vec<CClause>,
) -> Result<(Rc<CLoc>, CType), String> {
    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    let case_start_envs = TypeEnvs(envs.0.clone());

    // Pattern for each case must match the current session type labels.
    // We require only that the clauses available must be in the session type, not complete session type coverage (TODO TODO TODO is this sane?)
    for clause in clauses {
        // Find match in current session type
        let mut clause_envs = TypeEnvs(envs.0.clone());

        // Get the label for current patten
        let [CPat::Lit(_loc, label)] = clause.pats.as_slice() else {
            return Err("label must be an atom #1".to_string());
        };
        let Lit::Atom(label) = (**label).clone() else {
            return Err("label must be an atom #2".to_string());
        };

        // Look for label in session type
        // TODO: Can clone be avoided here?
        // TODO: Unused variable !
        let Some(_matching_st) = offers.get(&Label(label.0.clone())) else {
            return Err("No matching offer found in session!".to_string());
        };

        // TODO: It looks like I am missing something. Maybe base_expr var name? Or am I odd to
        // think so?

        // TODO: Potential optimization: Before vars are computed for each clause, where they could be computed outside loop.
        let clause_res = must_st_consume_expr(
            module,
            &case_start_envs,
            &mut clause_envs,
            cast_env,
            &clause.res,
        );
        if clause_res.is_err() {
            return Err(format!(
                "Case clause failed because {}",
                clause_res.err().unwrap()
            ));
        }

        // Add return type to check later
        // TODO: Check common_return_type in each iteration instead of later.
        common_return_type.push(clause_res.clone().unwrap().1);
    }

    // Check return-type is the same.
    let common_return_type_base = common_return_type.first().unwrap();
    common_return_type
        .iter()
        .all(|item| *item == *common_return_type_base);

    Ok((loc.clone(), common_return_type_base.clone()))
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
    // TODO: Maybe deep copy clone below can be avoided?
    let before_vars = before_envs.0.keys().map(|k| (**k).clone()).collect();
    let after_vars: HashSet<Var> = after_envs.0.keys().map(|k| (**k).clone()).collect();
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
                    if *check_consumed.0.first().unwrap() == SessionType::Cut {
                        continue;
                    }
                    return Err(format!(
                        "{:?} Not consumed #1 {:?}",
                        check_var, check_consumed
                    ));
                }
                return Err(format!(
                    "{:?} Not consumed #2 {:?}",
                    check_var, check_consumed
                ));
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
    cast_env: &mut CastEnv,
    e: &CExpr,
) -> Result<(Rc<CLoc>, CType), String> {
    // Check e2 in double mark
    match expr(module, current_envs, cast_env, e) {
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
    let new_vars: HashSet<Rc<Var>> = new_envs.0.keys().cloned().collect();
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

pub fn unfold(input: &SessionTypesList) -> SessionTypesList {
    // First onfold once
    let out = unfold_once(input);

    // Then check and unfold again if possible
    if let Some(SessionType::Rec(_)) = out.0.first() {
        unfold(&out)
    } else {
        out
    }
}

fn unfold_once(input: &SessionTypesList) -> SessionTypesList {
    match input.0.as_slice() {
        [SessionType::Rec(binder), inner @ ..] => {
            let free_names = free_names(inner);
            substitution(binder, &input.0, inner, &free_names.0)
        }
        _ => input.clone(), // TODO: Avoid clone here should be possible
    }
}

#[derive(Debug)]
struct FreeNames(Vec<FreeName>);

#[derive(Debug)]
enum FreeName {
    None,
    Free(Rc<Var>),
    Branch(HashMap<Label, FreeNames>),
}

fn free_names(input: &[SessionType]) -> FreeNames {
    let mut bound: HashSet<Rc<Var>> = HashSet::new();
    free_names_aux(&mut bound, input)
}

// The concept is give the free variable at each element in the same order as original to rematch.
// TODO: Right now shadowing is handled by marking the variable as bound. Maybe rename is better?
fn free_names_aux(bound: &mut HashSet<Rc<Var>>, input: &[SessionType]) -> FreeNames {
    match input {
        [] => FreeNames(vec![]),
        [SessionType::Var(var), tail @ ..] if tail.is_empty() => {
            let mut output: Vec<FreeName> = Vec::new();
            if bound.contains(var) {
                output.push(FreeName::None)
            } else {
                output.push(FreeName::Free(var.clone()));
            }
            let mut tail = free_names_aux(bound, tail).0;
            output.append(&mut tail);
            FreeNames(output)
        }
        [SessionType::Rec(var), tail @ ..] => {
            let mut output: Vec<FreeName> = Vec::new();
            bound.insert(var.clone());
            output.push(FreeName::None);
            let mut tail = free_names_aux(bound, tail).0;
            output.append(&mut tail);
            FreeNames(output)
        }
        [SessionType::End] => FreeNames(vec![FreeName::None]),
        [SessionType::Cut] => FreeNames(vec![FreeName::None]),
        [SessionType::Send(_), tail @ ..] => {
            let mut output: Vec<FreeName> = Vec::new();
            output.push(FreeName::None);
            let mut tail = free_names_aux(bound, tail).0;
            output.append(&mut tail);
            FreeNames(output)
        }
        [SessionType::Receive(_), tail @ ..] => {
            let mut output: Vec<FreeName> = Vec::new();
            output.push(FreeName::None);
            let mut tail = free_names_aux(bound, tail).0;
            output.append(&mut tail);
            FreeNames(output)
        }
        [SessionType::State(_), tail @ ..] => {
            let mut output: Vec<FreeName> = Vec::new();
            output.push(FreeName::None);
            let mut tail = free_names_aux(bound, tail).0;
            output.append(&mut tail);
            FreeNames(output)
        }
        [SessionType::Choice(_, choices)] => {
            let mut branch: HashMap<Label, FreeNames> = HashMap::new();
            for (label, choice) in choices {
                let res = free_names_aux(&mut bound.clone(), choice.0.as_slice());
                branch.insert(label.clone(), res);
            }
            let output: Vec<FreeName> = vec![FreeName::Branch(branch)];
            FreeNames(output)
        }
        x => todo!(
            "Invalid session type not supported by free name lookup: {:?}",
            x
        ),
    }
}

// Replacment in place
fn substitution(
    binder: &Rc<Var>,
    full: &[SessionType],
    input: &[SessionType],
    free_names: &[FreeName],
) -> SessionTypesList {
    match (input, free_names) {
        ([], []) => SessionTypesList(vec![]),
        ([SessionType::Var(var), tail @ ..], [FreeName::Free(free_var), tail_names @ ..])
            if var == binder && free_var == var && tail.is_empty() =>
        {
            let mut output: Vec<SessionType> = Vec::new();
            output.extend_from_slice(full);
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::Var(var), tail @ ..], [_, tail_names @ ..]) if tail.is_empty() => {
            let mut output: Vec<SessionType> = Vec::new();
            output.push(SessionType::Var(var.clone()));
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::Rec(var), tail @ ..], [_, tail_names @ ..]) => {
            let mut output: Vec<SessionType> = vec![SessionType::Rec(var.clone())];
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::End], [_]) => SessionTypesList(vec![SessionType::End]),
        ([SessionType::Cut], [_]) => SessionTypesList(vec![SessionType::Cut]),
        ([SessionType::Send(send), tail @ ..], [_, tail_names @ ..]) => {
            let mut output: Vec<SessionType> = Vec::new();
            output.push(SessionType::Send(send.clone()));
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::Receive(receive), tail @ ..], [_, tail_names @ ..]) => {
            let mut output: Vec<SessionType> = Vec::new();
            output.push(SessionType::Receive(receive.clone()));
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::State(state), tail @ ..], [_, tail_names @ ..]) => {
            let mut output: Vec<SessionType> = Vec::new();
            output.push(SessionType::State(state.clone()));
            let mut tail = substitution(binder, full, tail, tail_names).0;
            output.append(&mut tail);
            SessionTypesList(output)
        }
        ([SessionType::Choice(ct, choices)], [FreeName::Branch(names_map)]) => {
            let mut branch: BTreeMap<Label, SessionTypesList> = BTreeMap::new();
            for (label, choice) in choices {
                let this_name = names_map.get(label).unwrap();
                let res = substitution(binder, full, choice.0.as_slice(), this_name.0.as_slice());
                branch.insert(label.clone(), res);
            }
            let output: Vec<SessionType> = vec![SessionType::Choice(ct.clone(), branch)];
            SessionTypesList(output)
        }
        _ => todo!("Invalid session type not supported by substitution."),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct EqualityPairs(BTreeSet<BTreeSet<SessionTypesList>>);

fn equality(s1: &[SessionType], s2: &[SessionType]) -> bool {
    let seen_pairs = BTreeSet::new();
    equality_aux(&mut EqualityPairs(seen_pairs), s1, s2)
}

fn equality_aux(seen_pairs: &mut EqualityPairs, s1: &[SessionType], s2: &[SessionType]) -> bool {
    let mut current_pair = BTreeSet::new();
    current_pair.insert(SessionTypesList(s1.to_vec()));
    current_pair.insert(SessionTypesList(s2.to_vec()));
    if seen_pairs.0.contains(&current_pair) {
        return true;
    }
    seen_pairs.0.insert(current_pair);

    // Unfold if needed , unfold_once does nothing if unfold is not needed
    let s1 = unfold(&SessionTypesList(s1.to_vec())).0;
    let s1 = s1.as_slice();
    let s2 = unfold(&SessionTypesList(s2.to_vec())).0;
    let s2 = s2.as_slice();

    match (s1, s2) {
        ([SessionType::Choice(ct1, s1_choices)], [SessionType::Choice(ct2, s2_choices)])
            if ct1 == ct2 =>
        {
            if s1_choices.len() != s2_choices.len() {
                return false;
            }
            for (s1_elm_label, s1_elm_tail) in s1_choices {
                let Some(s2_elm_tail) = s2_choices.get(s1_elm_label) else {
                    return false;
                };
                if !equality_aux(&mut seen_pairs.clone(), &s1_elm_tail.0, &s2_elm_tail.0) {
                    return false;
                }
            }
            true
        }
        ([s1_head, s1_tail @ ..], [s2_head, s2_tail @ ..]) if s1_head == s2_head => {
            equality_aux(seen_pairs, s1_tail, s2_tail)
        }
        ([s1_head], [s2_head]) if s1_head == s2_head => true,
        ([], []) => true,
        x => {
            println!("DEBUG Rejected this {:?}", x);
            false
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn unfold_once_test_00() {
        // Session Type without recursion.
        let input = SessionTypesList(vec![SessionType::Send(BaseType::Any), SessionType::End]);

        // No change expected
        assert_eq!(unfold_once(&input.clone()), input);
    }

    #[test]
    fn unfold_once_test_01() {
        // Session Type without recursion.
        let input = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Any),
            SessionType::Var(Var("t".to_string()).into()),
        ]);

        // Change is expected. Nothing changed == bad
        assert_ne!(unfold_once(&input.clone()), input);

        // Expeted change:
        let expected = SessionTypesList(vec![
            SessionType::Send(BaseType::Any),
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Any),
            SessionType::Var(Var("t".to_string()).into()),
        ]);

        // Check expected is the output:
        assert_eq!(unfold_once(&input.clone()), expected);
    }

    #[test]
    fn unfold_once_test_02() {
        // E.g.:      rec t. !int. t.
        // Would be:         !int. rec t !int. t.
        // Session Type without recursion.
        let input = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);

        // Expeted change:
        let expected = SessionTypesList(vec![
            SessionType::Send(BaseType::Integer),
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);

        // Check expected is the output:
        assert_eq!(unfold_once(&input.clone()), expected);
    }

    #[test]
    #[should_panic]
    fn unfold_many_test_00() {
        // Not wellformed, must panic
        let input = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Rec(Var("u".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("u".to_string()).into()),
            SessionType::Var(Var("t".to_string()).into()),
        ]);

        // Expect panic here:
        unfold(&input);
    }

    #[test]
    #[should_panic]
    fn unfold_many_test_01() {
        // Not wellformed, must panic
        let input = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Rec(Var("u".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
            SessionType::Var(Var("u".to_string()).into()),
        ]);

        // Expect panic here:
        unfold(&input);
    }

    #[test]
    fn unfold_many_test_02() {
        let input = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Rec(Var("u".to_string()).into()),
            SessionType::Choice(
                ChoiceType::Make,
                BTreeMap::from([
                    (
                        Label("first".to_owned()),
                        SessionTypesList(vec![
                            SessionType::Send(BaseType::Integer),
                            SessionType::Var(Var("t".to_string()).into()),
                        ]),
                    ),
                    (
                        Label("second".to_owned()),
                        SessionTypesList(vec![
                            SessionType::Send(BaseType::String),
                            SessionType::Var(Var("u".to_string()).into()),
                        ]),
                    ),
                ]),
            ),
        ]);

        // Expeted change:
        let expected = SessionTypesList(vec![SessionType::Choice(
            ChoiceType::Make,
            BTreeMap::from([
                (
                    Label("first".to_owned()),
                    SessionTypesList(vec![
                        SessionType::Send(BaseType::Integer),
                        SessionType::Rec(Var("t".to_string()).into()),
                        SessionType::Rec(Var("u".to_string()).into()),
                        SessionType::Choice(
                            ChoiceType::Make,
                            BTreeMap::from([
                                (
                                    Label("first".to_owned()),
                                    SessionTypesList(vec![
                                        SessionType::Send(BaseType::Integer),
                                        SessionType::Var(Var("t".to_string()).into()),
                                    ]),
                                ),
                                (
                                    Label("second".to_owned()),
                                    SessionTypesList(vec![
                                        SessionType::Send(BaseType::String),
                                        SessionType::Var(Var("u".to_string()).into()),
                                    ]),
                                ),
                            ]),
                        ),
                    ]),
                ),
                (
                    Label("second".to_owned()),
                    SessionTypesList(vec![
                        SessionType::Send(BaseType::String),
                        SessionType::Rec(Var("u".to_string()).into()),
                        SessionType::Choice(
                            ChoiceType::Make,
                            BTreeMap::from([
                                (
                                    Label("first".to_owned()),
                                    SessionTypesList(vec![
                                        SessionType::Send(BaseType::Integer),
                                        SessionType::Rec(Var("t".to_string()).into()),
                                        SessionType::Rec(Var("u".to_string()).into()),
                                        SessionType::Choice(
                                            ChoiceType::Make,
                                            BTreeMap::from([
                                                (
                                                    Label("first".to_owned()),
                                                    SessionTypesList(vec![
                                                        SessionType::Send(BaseType::Integer),
                                                        SessionType::Var(
                                                            Var("t".to_string()).into(),
                                                        ),
                                                    ]),
                                                ),
                                                (
                                                    Label("second".to_owned()),
                                                    SessionTypesList(vec![
                                                        SessionType::Send(BaseType::String),
                                                        SessionType::Var(
                                                            Var("u".to_string()).into(),
                                                        ),
                                                    ]),
                                                ),
                                            ]),
                                        ),
                                    ]),
                                ),
                                (
                                    Label("second".to_owned()),
                                    SessionTypesList(vec![
                                        SessionType::Send(BaseType::String),
                                        SessionType::Var(Var("u".to_string()).into()),
                                    ]),
                                ),
                            ]),
                        ),
                    ]),
                ),
            ]),
        )]);

        // Check expected is the output:
        assert_eq!(unfold(&input.clone()), expected);
    }

    #[test]
    fn equality_test_00() {
        // Sanity check: If we clone the input it should be equal.
        let input = SessionTypesList(vec![SessionType::End]);
        assert!(equality(&input.clone().0, &input.0));
    }

    #[test]
    fn equality_test_01() {
        // Simple recursive type check: rec t. !int t. = rec t. !int !int t.
        let s1 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        let s2 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        assert!(equality(&s1.0, &s2.0));
    }

    #[test]
    fn equality_test_02() {
        // Sanity check: rec t. !int t. != rec t. !int ?int t.
        let s1 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        let s2 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Receive(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        assert!(!equality(&s1.0, &s2.0));
    }

    #[test]
    fn equality_test_03() {
        // Simple recursive type check: rec t. !int t. = rec u. !int !int u.
        let s1 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        let s2 = SessionTypesList(vec![
            SessionType::Rec(Var("u".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("u".to_string()).into()),
        ]);
        assert!(equality(&s1.0, &s2.0));
    }

    #[test]
    #[should_panic]
    fn equality_test_04() {
        // Test double unfold: rec t. rec u. !int u. t. = rec t. !int !int t.
        // This must panic, it is nonsense to do this!
        let s1 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Rec(Var("u".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
            SessionType::Var(Var("u".to_string()).into()),
        ]);
        let s2 = SessionTypesList(vec![
            SessionType::Rec(Var("t".to_string()).into()),
            SessionType::Send(BaseType::Integer),
            SessionType::Send(BaseType::Integer),
            SessionType::Var(Var("t".to_string()).into()),
        ]);
        assert!(equality(&s1.0, &s2.0));
    }
}
