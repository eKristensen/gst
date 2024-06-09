use crate::{
    cerl_parser::ast::{Atom, FunName},
    contract_cerl::{
        ast::{CExpr, CFunCall, CModule, CType},
        types::{BaseType, SessionType, SessionTypesList},
    },
    type_checker::env::TypeEnv,
};

use super::{env::TypeEnvs, session::must_st_consume_expr};

// Handle build in functions
pub fn bif_fun(
    // module: &CModule,
    // envs: &mut TypeEnvs,
    call: &CFunCall,
    // args: &Vec<CExpr>,
) -> Result<CType, String> {
    // TODO: More clever way to handle BIF
    let bif_io_format = CFunCall::Call(Atom("io".to_owned()), Atom("format".to_owned()));
    if *call == bif_io_format {
        return Ok(CType::Base(BaseType::Atom(Atom("ok".to_string()))));
    }

    Err("Not bif".to_string())
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
            name
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
            name
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
            (CType::Consume(_, _), CType::Consume(_, _)) => continue, // Do not check session type content now. Later!
            _ => return false,                                        // mismatch not ok.
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
            (CType::Consume(_, to_consume), CType::Consume(_, source_session)) => {
                // do the consume
                let updated_source_session =
                    st_consume_single_path(source_session.0.as_slice(), to_consume.0.as_slice())?;

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
        SessionType::MakeChoice(_, choice) => {
            is_st_subtype_aux(t1, choice.0.as_slice()) && t2.len() == 1
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
    }
}

// Consume from session if possible on a single path based on inputs
fn st_consume_single_path(
    source_session: &[SessionType],
    to_consume: &[SessionType],
) -> Result<SessionTypesList, String> {
    if to_consume.is_empty() {
        return Ok(SessionTypesList(source_session.to_vec()));
    }
    if source_session.is_empty() {
        return Err("Cannot consume on from consumed session".to_string());
    }
    match (source_session.first().unwrap(), to_consume.first().unwrap()) {
        (SessionType::Send(t1), SessionType::Send(t2)) => {
            if *t1 != *t2 {
                return Err("Consume type mismatch".to_string());
            }
            st_consume_single_path(&source_session[1..], &to_consume[1..])
        }
        (SessionType::Receive(t1), SessionType::Receive(t2)) => {
            if *t1 != *t2 {
                return Err("Consume type mismatch".to_string());
            }
            st_consume_single_path(&source_session[1..], &to_consume[1..])
        }
        (SessionType::MakeChoice(l1, t1), SessionType::MakeChoice(l2, t2)) => {
            if *l1 != *l2 {
                return Err("Label mismatch".to_string());
            }
            if source_session.len() != 1 || to_consume.len() != 1 {
                return Err("session type structure invalid".to_string());
            }
            st_consume_single_path(t1.0.as_slice(), t2.0.as_slice())
        }
        (SessionType::OfferChoice(_), SessionType::OfferChoice(_)) => todo!(),
        (SessionType::End, SessionType::End) => Ok(SessionTypesList(source_session.to_vec())),
        _ => Err("Consume not possible".to_string()),
    }
}
