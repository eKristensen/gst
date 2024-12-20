use std::{collections::BTreeSet, rc::Rc};

use crate::{
    cerl_parser::ast::{Atom, CLoc},
    contract_cerl::{
        ast::{CExpr, CFunClause, CModule, CType},
        types::{BaseType, ChoiceType, SessionType, SessionTypesList},
    },
    type_checker::{casting::add_gradual_cast, env::TypeEnv},
};

use super::{
    casting::get_cexpr_loc,
    env::{CastEnv, TypeEnvs},
    session::{must_st_consume_expr, unfold},
};

// Handle build in functions
pub fn bif_fun(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    fun_mod: &str,
    fun_name: &str,
    args: &[CExpr],
) -> Result<(Rc<CLoc>, CType), String> {
    match (fun_mod, fun_name) {
        ("io", "format") => Ok((
            loc.clone(),
            CType::Base(BaseType::Atom(Atom("ok".to_string()).into())),
        )),
        ("erlang", "-") => {
            // TODO: Add support for other types and binary. Only unary minus supported now
            if args.len() != 1 {
                return Err("Wrong or unknown use of erlang:-".to_string());
            }
            let val_in = args.first().unwrap();
            let type_in =
                must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, val_in)?;
            if CType::Base(BaseType::Integer) == type_in.1 {
                Ok((loc.clone(), CType::Base(BaseType::Integer)))
            } else if CType::Base(BaseType::Dynamic) == type_in.1 {
                add_gradual_cast(
                    cast_env,
                    &get_cexpr_loc(val_in),
                    &CType::Base(BaseType::Dynamic),
                    &CType::Base(BaseType::Integer),
                )?;
                Ok((loc.clone(), CType::Base(BaseType::Integer)))
            } else {
                Err("Expected integer for erlang:- function.".to_string())
            }
        }
        ("erlang", "+") => {
            if args.len() != 2 {
                return Err("Wrong or unknown use of erlang:+".to_string());
            }
            let [val_1, val_2] = args else {
                return Err("Expected two args for erlang:+".to_string());
            };
            let type_1_in =
                must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, val_1)?;
            let type_2_in =
                must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, val_2)?;
            add_gradual_cast(
                cast_env,
                &type_1_in.0,
                &type_1_in.1,
                &CType::Base(BaseType::Integer),
            )?;
            add_gradual_cast(
                cast_env,
                &type_2_in.0,
                &type_2_in.1,
                &CType::Base(BaseType::Integer),
            )?;
            Ok((loc.clone(), CType::Base(BaseType::Integer)))
        }
        ("gen_server_plus", "start_link") => {
            println!("Text-only warning: TODO: Proper return type when custom types are supported for gen_server_plus:start_link.");
            Ok((loc.clone(), CType::Base(BaseType::Any)))
        }
        _ => Err("Unknown bif or not bif".to_string()),
    }
}

// e_app
pub fn e_app(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    loc: &Rc<CLoc>,
    args: &[CExpr],
    fun_clauses: &Vec<CFunClause>,
) -> Result<(Rc<CLoc>, CType), String> {
    // 2) Use contract to check argument types, including session type evaluation
    // 2a) Get types of arguments
    let mut input_types: Vec<(Rc<CLoc>, CType)> = Vec::new();
    for elm in args {
        match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, elm) {
            Ok((_loc, ok_val)) => input_types.push((get_cexpr_loc(elm), ok_val)),
            Err(err_val) => {
                return Err(format!(
                    "Type checking argument in call failed because {}",
                    err_val
                ))
            }
        }
    }

    // 2b) Find the matching (typeable) function clause
    // First matching clause will be the chosen clause
    for clause in fun_clauses {
        // Find that clause that erlang would choose at runtime based on contract
        // TODO: Huge assumption: Spec reflects which clause of erlang that would match.
        //       Why ok? Because anything else would not reflect the real world at all.
        if !fun_app_clause_match(&clause.spec, &input_types) {
            continue;
        }

        // 2c) Apply clause contract, basically apply session consume
        if let Err(err_val) = e_app_contract(module, envs, cast_env, &clause.spec, args) {
            return Err(format!(
                "Could not apply function contract because {}",
                err_val
            ));
        }

        // 3) Return type is given by function contract
        return Ok((loc.clone(), CType::Base(clause.return_type.clone())));
    }

    // Attempt Gradual casting: If the input type is a gradual type, then we assume there is just one clause
    // to match and that this clause is the one where we need to perform the gradual casting
    // TODO: Write this assumption somewhere relevant, maybe in paper or documentation?
    if fun_clauses.len() == 1 && fun_clauses.first().unwrap().spec.len() == input_types.len() {
        let clause = fun_clauses.first().unwrap();

        let mut acceptable_cast = true;

        for elm in clause.spec.iter().zip(input_types.iter()) {
            match elm {
                (CType::Base(out), (cloc, CType::Base(BaseType::Dynamic))) => {
                    println!("INFO: Requested cast to be inserted.");
                    add_gradual_cast(
                        cast_env,
                        cloc,
                        &CType::Base(BaseType::Dynamic),
                        &CType::Base(out.clone()),
                    )?;
                }
                (CType::Base(t1), (_, CType::Base(t2))) => {
                    if *t1 != *t2 {
                        acceptable_cast = false;
                        break;
                    }
                }
                (CType::New(_), (_, CType::New(_))) => continue, // Do not check session type content now. Later!
                (CType::Consume(_), (_, CType::Consume(_))) => continue, // Do not check session type content now. Later!
                _ => {
                    acceptable_cast = false;
                    break;
                } // mismatch not ok.
            }
        }
        if acceptable_cast {
            return Ok((loc.clone(), CType::Base(clause.return_type.clone())));
        }
    }

    // No typeable function clause found
    Err("Found no function clause with matching type.".to_string())
}

// Check the input types are compatible with the contract.
// Do not check session type content.
fn fun_app_clause_match(type1: &[CType], type2: &[(Rc<CLoc>, CType)]) -> bool {
    if type1.len() != type2.len() {
        return false;
    }
    for elm in type1.iter().zip(type2.iter()) {
        match elm {
            (CType::Base(t1), (_, CType::Base(t2))) => {
                if *t1 != *t2 {
                    return false;
                }
            }
            (CType::New(_), (_, CType::New(_))) => continue, // Do not check session type content now. Later!
            (CType::Consume(_), (_, CType::Consume(_))) => continue, // Do not check session type content now. Later!
            _ => return false,                                       // mismatch not ok.
        }
    }
    true
}

// Apply/Evaluate function contract in environment
fn e_app_contract(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    contract: &[CType],
    inputs: &[CExpr],
) -> Result<(), String> {
    for (contract_elm, input_elm) in contract.iter().zip(inputs.iter()) {
        let (_loc, ctype_input_elm) = match must_st_consume_expr(
            module,
            &TypeEnvs(envs.0.clone()),
            envs,
            cast_env,
            input_elm,
        ) {
            Ok(ok_val) => ok_val,
            Err(err_val) => return Err(format!("Err finding argument type: {}", err_val)),
        };

        match (contract_elm, ctype_input_elm) {
            (CType::Base(t1), CType::Base(t2)) => {
                // Equality check
                if *t1 != t2 {
                    return Err(format!(
                        "Function contact not compatible. Expected {:?}, but input was {:?}",
                        t1, t2
                    ));
                }
            }
            (CType::New(t1), CType::New(t2)) => {
                if *t1 != t2 {
                    return Err("Cannot compare server session types. No subtype relation yet, new() type must be the same.".to_string());
                }
            }
            (CType::Consume(to_consume), CType::Consume(source_session)) => {
                // do the consume
                let updated_source_session =
                    st_consume(source_session.0.as_slice(), to_consume.0.as_slice())?;

                // update env, IMPORTANT!
                let CExpr::Var(_loc, session_id_var) = input_elm else {
                    return Err("Session identifier must be direct arg.".to_string());
                };

                envs.0.insert(
                    session_id_var.clone(),
                    TypeEnv::Delta(updated_source_session),
                );
            } // Consume session
            _ => return Err("Fun app contract mismatch".to_string()),
        }
    }
    // If no error was found, applying the function contract is considered successful.
    Ok(())
}

// Partial session equality relation
// t1 <: t2
fn is_st_subtype(t1: &SessionTypesList, t2: &SessionTypesList) -> bool {
    let mut seen_pairs = BTreeSet::new();
    is_st_subtype_aux(&mut seen_pairs, t1, t2)
}

// TODO How to handle "End" in subtype relation??
// Can subtype end before or can it only be done i supertype?
fn is_st_subtype_aux(
    seen_pairs: &mut BTreeSet<(SessionTypesList, SessionTypesList)>,
    t1: &SessionTypesList,
    t2: &SessionTypesList,
) -> bool {
    panic!("No subtyping yet!");
    // TODO: Unfold and memory...
    if seen_pairs.contains(&(t1.clone(), t2.clone())) {
        return true;
    }
    seen_pairs.insert((t1.clone(), t2.clone()));
    let t1 = unfold(t1).0;
    let t1 = t1.as_slice();

    let t2 = unfold(t2).0;
    let t2 = t2.as_slice();

    match (t1, t2) {
        (t1, t2) if t1 == t2 => true,
        ([SessionType::End], [SessionType::End]) => true,
        ([t1_head, t1_tail @ ..], [t2_head, t2_tail @ ..]) if t1_head == t2_head => {
            // TODO: General match t1_head==t2_head maybe allows too much? Should not be a problem
            // when wellformed?
            is_st_subtype_aux(
                seen_pairs,
                &SessionTypesList(t1_tail.to_vec()),
                &SessionTypesList(t2_tail.to_vec()),
            )
        }
        ([SessionType::Choice(ct1, t1_choices)], [SessionType::Choice(ct2, t2_choices)])
            if ct1 == ct2 =>
        {
            for (t1_label, t1_st) in t1_choices {
                if let Some(t2_st) = t2_choices.get(t1_label) {
                    if !is_st_subtype_aux(seen_pairs, t1_st, t2_st) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        }
        ([_head, _tail @ ..], []) => true,
        (_, _) => false,
    }
}

// Consume from session if possible on a single path based on inputs
fn st_consume(
    current_session: &[SessionType],
    to_consume: &[SessionType],
) -> Result<SessionTypesList, String> {
    let mut seen_pairs = BTreeSet::new();
    st_consume_aux(&mut seen_pairs, current_session, to_consume)
}

fn st_consume_aux(
    seen_pairs: &mut BTreeSet<(Vec<SessionType>, Vec<SessionType>)>,
    current_session: &[SessionType],
    to_consume: &[SessionType],
) -> Result<SessionTypesList, String> {
    // TODO: Wellformed check to ensure nothing comes after "end".

    // Unfold first ?
    let current_session = unfold(&SessionTypesList(current_session.to_vec())).0;
    let to_consume = unfold(&SessionTypesList(to_consume.to_vec())).0;

    let pair = (current_session.clone(), to_consume.clone());
    if seen_pairs.contains(&pair) {
        return Ok(SessionTypesList(current_session));
    } else {
        seen_pairs.insert(pair);
    }
    let current_session = current_session.as_slice();
    let to_consume = to_consume.as_slice();

    match (current_session, to_consume) {
        // TODO: Should cut be required instead of allow "emptyness" to define the end of a
        // consume? Right now both are allowed in this type checker...
        (_, []) => Ok(SessionTypesList(current_session.to_vec())),
        (_, [SessionType::Cut]) => Ok(SessionTypesList(current_session.to_vec())),
        ([SessionType::End], [SessionType::End]) => Ok(SessionTypesList(vec![])),
        ([SessionType::Choice(ct1, cur_choices)], [SessionType::Choice(ct2, consume_choices)])
            if ct1 == ct2 && *ct1 == ChoiceType::Make =>
        {
            // TODO: Subtyping. For now we require the same labels on both sides
            // One way is checked in the session type consume check, here we check the reverse. If
            // both are equal the same labels exists both ways
            for label in cur_choices.keys() {
                if !consume_choices.contains_key(label) {
                    return Err(
                        "Labels on both sides must be equal. No subtype support yet.".to_string(),
                    );
                }
            }
            // For all choices in contract, the current session must match. After each branch the
            // session must be left the same/have a common return type, else fail the consume.
            // It is fine if consume does not have all the labels that the current session has, but
            // the labels that consume can chose MUST be in the current session.
            let mut common_return = None;
            for (consume_choice_label, consume_choice_tail) in consume_choices {
                let Some(cur_choice_session_tail) = cur_choices.get(consume_choice_label) else {
                    return Err(format!(
                        "Required label {:?} in order to consume {:?} from {:?}",
                        consume_choice_label, to_consume, current_session
                    ));
                };
                let res = st_consume_aux(
                    seen_pairs,
                    cur_choice_session_tail.0.as_slice(),
                    consume_choice_tail.0.as_slice(),
                )?;
                match &common_return {
                    // Do we already know the session that we are supposed to see?
                    Some(common_return) => {
                        if common_return != &res {
                            return Err(format!(
                                "Branching session type must result in a common residual type. Expected {:?} but found {:?}",
                                common_return, res),
                            );
                        }
                    }
                    // If not, then it is the first round:
                    // On first round learn the sessiontype for the common return
                    None => common_return = Some(res),
                }
            }
            // Return the state consume would leave the session in no matter what choice that was
            // made.
            match &common_return {
                Some(common_return) => Ok(common_return.clone()),
                None => Err("No common return. No choices available?".to_string()),
            }
        }
        ([cur_head, cur_tail @ ..], [consume_head, consume_tail @ ..])
            if cur_head == consume_head =>
        {
            st_consume_aux(seen_pairs, cur_tail, consume_tail)
        }
        _ => Err(format!(
            "Could not consume {:?} from {:?}",
            to_consume, current_session
        )),
    }
}
