use std::collections::BTreeSet;

use crate::{
    cerl_parser::ast::{Atom, FunName},
    contract_cerl::{
        ast::{CExpr, CFunCall, CModule, CType},
        types::{BaseType, SessionType, SessionTypesList},
    },
    type_checker::env::TypeEnv,
};

use super::{
    env::TypeEnvs,
    session::{must_st_consume_expr, unfold},
};

// Handle build in functions
pub fn bif_fun(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    let CFunCall::Call(fun_mod, fun_name) = call else {
        return Err("Unknown format bif or not bif".to_string());
    };
    match (fun_mod.0.as_str(), fun_name.0.as_str()) {
        ("io", "format") => Ok(CType::Base(BaseType::Atom(Atom("ok".to_string())))),
        ("erlang", "-") => {
            // TODO: Add support for other types and binary. Only unary minus supported now
            if args.len() != 1 {
                return Err("Wrong or unknown use of erlang:-".to_string());
            }
            let val_in = args.first().unwrap();
            let type_in = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, val_in)?;
            if CType::Base(BaseType::Integer) == type_in {
                Ok(CType::Base(BaseType::Integer))
            } else {
                Err("Expected integer for erlang:- function.".to_string())
            }
        }
        ("erlang", "+") => {
            if args.len() != 2 {
                return Err("Wrong or unknown use of erlang:+".to_string());
            }
            let [val_1, val_2] = args.as_slice() else {
                return Err("Expected two args for erlang:+".to_string());
            };
            println!("Am here: {:?} {:?}", val_1, envs);
            let type_1_in = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, val_1)?;
            println!("Am here 2");
            let type_2_in = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, val_2)?;
            println!("Am here 3");
            if CType::Base(BaseType::Integer) == type_1_in && type_1_in == type_2_in {
                Ok(CType::Base(BaseType::Integer))
            } else {
                Err("Expected integer for erlang:+ function.".to_string())
            }
        }
        ("gen_server_plus", "start_link") => {
            println!("Text-only warning: TODO: Proper return type when custom types are supported for gen_server_plus:start_link.");
            Ok(CType::Base(BaseType::Any))
        }
        _ => Err("Unknown bif or not bif".to_string()),
    }
}

// e_app
pub fn e_app(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    // TODO: Improvement: Inter-module contract lookup
    // 1) Lookup call to find function contract
    let fun_name = match call {
        CFunCall::PrimOp(_) => return Err("Function application cannot be a PrimOp.".to_string()),
        CFunCall::Apply(name) => {
            // Function call to function in the same module
            name.name.clone()
        }
        CFunCall::Call(call_module_name, name) => {
            // Function call potentially to another module or the same
            // We currently only support calls to the same module.
            // First step: Check module name matches
            // Otherwise the same as above.
            if module.name != *call_module_name {
                return Err("Only call to the same module is supported currently".to_string());
            }

            // If OK, then return name.
            name.clone()
        }
    };

    // Lookup function clauses
    let Some(fun_clauses) = module.functions.get(&FunName {
        name: fun_name.clone(),
        arity: args.len(),
    }) else {
        return Err("Function application not possible as function name not found".to_string());
    };

    // 2) Use contract to check argument types, including session type evaluation
    // 2a) Get types of arguments
    let mut input_types: Vec<CType> = Vec::new();
    for elm in args {
        println!("must_st_consume_expr called by e_app");
        match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, elm) {
            Ok(ok_val) => input_types.push(ok_val),
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
        if let Err(err_val) = e_app_contract(module, envs, &clause.spec, args) {
            return Err(format!(
                "Could not apply function contract because {}",
                err_val
            ));
        }

        // 3) Return type is given by function contract
        return Ok(CType::Base(clause.return_type.clone()));
    }

    // No typeable function clause found
    Err("Found no function clause with matching type.".to_string())
}

// Check the input types are compatible with the contract.
// Do not check session type content.
fn fun_app_clause_match(type1: &[CType], type2: &[CType]) -> bool {
    if type1.len() != type2.len() {
        return false;
    }
    for elm in type1.iter().zip(type2.iter()) {
        match elm {
            (CType::Base(t1), CType::Base(t2)) => {
                if *t1 != *t2 {
                    return false;
                }
            }
            (CType::New(_), CType::New(_)) => continue, // Do not check session type content now. Later!
            (CType::Consume(_), CType::Consume(_)) => continue, // Do not check session type content now. Later!
            _ => return false,                                  // mismatch not ok.
        }
    }
    true
}

// Apply/Evaluate function contract in environment
fn e_app_contract(
    module: &CModule,
    envs: &mut TypeEnvs,
    contract: &[CType],
    inputs: &[CExpr],
) -> Result<(), String> {
    for (contract_elm, input_elm) in contract.iter().zip(inputs.iter()) {
        println!("must_st_consume_expr called by e_app_contract");
        let ctype_input_elm =
            match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, input_elm) {
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
                if !is_st_subtype(t1, &t2) {
                    return Err("Partial session equality relation failed".to_string());
                }
            } // Subtype check
            (CType::Consume(to_consume), CType::Consume(source_session)) => {
                // do the consume
                let updated_source_session =
                    st_consume(source_session.0.as_slice(), to_consume.0.as_slice())?;

                // update env, IMPORTANT!
                let CExpr::Var(session_id_var) = input_elm else {
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
    is_st_subtype_aux(t1.0.as_slice(), t2.0.as_slice())
}

// TODO How to handle "End" in subtype relation??
// Can subtype end before or can it only be done i supertype?
fn is_st_subtype_aux(t1: &[SessionType], t2: &[SessionType]) -> bool {
    if t2.is_empty() && !t1.is_empty() {
        return false;
    };
    if t1.is_empty() && t2.is_empty() {
        return false;
    };
    if t1 == t2 {
        return true;
    };
    if t1 == [SessionType::End] && t2.is_empty() {
        return false;
    };
    match t2.first().unwrap() {
        SessionType::Send(_) => is_st_subtype_aux(t1, &t2[1..]),
        SessionType::Receive(_) => is_st_subtype_aux(t1, &t2[1..]),
        SessionType::MakeChoice(choices) => {
            if t2.len() != 1 {
                return false;
            }
            for choice in choices.values() {
                if is_st_subtype_aux(t1, choice.0.as_slice()) {
                    return true;
                }
            }
            false
        }
        SessionType::OfferChoice(offers) => {
            if t2.len() != 1 {
                return false;
            }
            for offer in offers.values() {
                if is_st_subtype_aux(t1, offer.0.as_slice()) {
                    return true;
                }
            }
            false
        }
        SessionType::End => false,
        SessionType::State(_) => todo!("State is not allowed in -session. Properly handle error message or redesign to not need it."),
        SessionType::Var(_) => todo!(),
        SessionType::Rec(_,_) => todo!(),
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
    println!("st consume unfolding");
    let current_session = unfold(&SessionTypesList(current_session.to_vec())).0;
    let to_consume = unfold(&SessionTypesList(to_consume.to_vec())).0;

    let pair = (current_session.clone(), to_consume.clone());
    if seen_pairs.contains(&pair) {
        println!("Found pair, returning current session");
        return Ok(SessionTypesList(current_session));
    } else {
        seen_pairs.insert(pair);
    }
    let current_session = current_session.as_slice();
    let to_consume = to_consume.as_slice();

    match (current_session, to_consume) {
        (_, []) => Ok(SessionTypesList(current_session.to_vec())),
        ([SessionType::End], [SessionType::End]) => Ok(SessionTypesList(vec![SessionType::End])),
        ([SessionType::MakeChoice(cur_choices)], [SessionType::MakeChoice(consume_choices)]) => {
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
