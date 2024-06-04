use crate::{
    cerl_parser::ast::{Atom, Lit, Pat, Var},
    contract_cerl::{
        ast::{CExpr, CFunCall, CModule, CType},
        types::{BaseType, Label, SessionType},
    },
};

use super::{
    env::{TypeEnv, TypeEnvs},
    session::{e_case, must_st_consume_expr},
};

pub fn expr(module: &CModule, envs: &mut TypeEnvs, e: &CExpr) -> Result<CType, String> {
    match e {
        CExpr::Var(v) => e_base(envs, v),
        CExpr::Lit(l) => Ok(e_lit(l)),
        CExpr::Cons(_) => todo!(),
        CExpr::Tuple(_) => todo!(),
        CExpr::Let(v, e1, e2) => e_let(module, envs, v, e1, e2),
        CExpr::Case(base_expr, clauses) => e_case(module, envs, base_expr, clauses),
        CExpr::Call(call, args) => e_call(module, envs, call, args),
        CExpr::Do(e1, e2) => e_do(module, envs, e1, e2),
    }
}

// TODO: e-call function is too big, split into smaller parts.
fn e_call(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    // Call can be both a send operation and a "normal" call.
    // We need to check and perform the right kind of type check
    // 1) A GS+ call
    // 1a) Send base value  (e_send)
    // 1b) Send make choice (e_select)
    // 2)  A native call    (e_app, the fallback)

    let gsp_sync_send = CFunCall::Call(Atom("gen_server_plus".to_owned()), Atom("call".to_owned()));
    let gsp_new = CFunCall::Call(Atom("gen_server_plus".to_owned()), Atom("new".to_owned()));

    if *call == gsp_sync_send {
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
        let CType::Base(sending_val) =
            (match expr(module, &mut TypeEnvs(envs.0.clone()), sending_expr) {
                Ok(ok_val) => ok_val,
                Err(err_val) => {
                    return Err(format!("e_call gsp_sync_send failed because {}", err_val))
                }
            })
        else {
            return Err("e_call gsp_sync_send can only send base values".to_string());
        };
        //println!("{:?}", envs);

        // Get current session
        let session_id = &args[1];
        let CExpr::Var(session_var) = session_id else {
            return Err(
                "e_call gsp_sync_send Session variable name must be used, not expression"
                    .to_string(),
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
                return Ok(CType::Base(received));
            }
            SessionType::Receive(_) => {
                return Err("Session type says receive, we are about to send".to_string())
            }
            SessionType::MakeChoice(_, _) => {
                return Err("Session type MakeChoice, expected OfferChoice".to_string())
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
                        return Ok(CType::Consume(
                            Some(session_var.clone()),
                            continuation.clone(),
                        ));
                    }
                    None => return Err("Trying to make choice not offered by session".to_string()),
                }
            }
            SessionType::End => {
                return Err("Session type is End, but we are about to use it".to_string())
            }
        }

        // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
        // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
        // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
        // Generic "check env before-after" is strongly needed now!

        // We can only send base values

        // If it is not a base-value we assume it is select
    }

    if *call == gsp_new {
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
        let CType::New(session_type) =
            (match expr(module, &mut TypeEnvs(envs.0.clone()), server_pid) {
                Ok(ok_val) => ok_val,
                Err(err_val) => return Err(format!("E_call gsp_new failed due to {}", err_val)),
            })
        else {
            return Err("Must construct new session here".to_string());
        };
        // println!("type of pid {:?}", session_type);

        return Ok(CType::Consume(None, session_type));

        // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
        // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
        // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
        // Generic "check env before-after" is strongly needed now!

        // We can only send base values

        // If it is not a base-value we assume it is select
    }

    // TODO: More clever way to handle BIF
    let bif_io_format = CFunCall::Call(Atom("io".to_owned()), Atom("format".to_owned()));
    if *call == bif_io_format {
        return Ok(CType::Base(BaseType::Atom(Atom("ok".to_string()))));
    }

    // println!("{:?} {:?}", call, args);
    todo!()
}

fn e_base(envs: &TypeEnvs, v: &Var) -> Result<CType, String> {
    let res = envs.0.get(v);
    if res.is_none() {
        return Err(format!("used var that was not defined {:?}", v));
    }
    match res.unwrap() {
        // TODO: How to allow both base value checked properly, and not to "return" a base type?
        TypeEnv::Gamma(n) => Ok(CType::New(n.clone())),
        TypeEnv::Delta(c) => Ok(CType::Consume(None, c.clone())),
        TypeEnv::Sigma(res) => Ok(CType::Base(res.clone())),
    }
}

fn e_lit(l: &Lit) -> CType {
    match l {
        Lit::Int(_) => CType::Base(BaseType::Integer),
        Lit::Float(_) => CType::Base(BaseType::Float),
        Lit::Atom(a) => CType::Base(BaseType::Atom(a.clone())),
        Lit::Char(_) => CType::Base(BaseType::Char),
        Lit::Cons(_) => todo!(),
        Lit::Tuple(_) => todo!(),
        Lit::String(_) => CType::Base(BaseType::String),
    }
}

fn e_do(module: &CModule, envs: &mut TypeEnvs, e1: &CExpr, e2: &CExpr) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let eval_ok = expr(module, envs, e1);
    if eval_ok.is_err() {
        return Err(format!("e_do failed because {}", eval_ok.err().unwrap()));
    }
    // println!("DO must consume");
    match must_st_consume_expr(module, &envs_copy_baseline, envs, e2) {
        Ok(ok_val) => Ok(ok_val),
        Err(err_val) => Err(format!("e_do failed because {}", err_val)),
    }
}

// TODO: Reuse this function for : E_case, E_app ("call-by-value")
// Evaluate type in an isolated environment and ensure there are no lose ends
// Almost like expr function with an additional environment check
// Models call-by-value in Erlang. Isolated evaluation, i.e environment is used as base, but not changed.
fn e_let(
    module: &CModule,
    envs: &mut TypeEnvs,
    v: &Pat,
    e1: &CExpr,
    e2: &CExpr,
) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let pat_ok = pattern_matching(module, envs, v, e1);
    if pat_ok.is_err() {
        return Err(format!(
            "e_let ({:?} {:?} {:?}) failed #1 because {}",
            v,
            e1,
            e2,
            pat_ok.err().unwrap()
        ));
    }

    // println!("LET must consume");
    match must_st_consume_expr(module, &envs_copy_baseline, envs, e2) {
        Ok(ok_val) => Ok(ok_val),
        Err(err_val) => Err(format!(
            "e_let \n1) {:?}\n2) {:?}\n3) {:?}) \n failed #2 because {}",
            v, e1, e2, err_val
        )),
    }
}

// Aka: unification
// https://en.wikipedia.org/wiki/Unification_(computer_science)#Unification_algorithms
fn pattern_matching(
    module: &CModule,
    envs: &mut TypeEnvs,
    v: &Pat,
    e: &CExpr,
) -> Result<(), String> {
    match v {
        Pat::Var(v) => {
            // TODO: No explicit isolation of env here. Is properly as it should be...
            // Get expression type
            let t = expr(module, envs, e);
            if t.is_err() {
                return Err(format!(
                    "pattern matching failed in var because {}",
                    t.err().unwrap()
                ));
            }
            // TODO: Add "remove if equal"/NOOP thing
            envs.0.insert(v.clone(), ctype_to_typeenv(&t.unwrap()));
            Ok(())
        }
        Pat::Lit(l) => {
            // Must match on lit
            let CExpr::Lit(e_lit) = e else {
                return Err(
                    "Lit on left hand side can only be unified with lit on right hand side"
                        .to_string(),
                );
            };
            // They must be equal, if not return error
            if *l != *e_lit {
                return Err(
                    "pattern matching failed in Lit must be equal in pattern matching.".to_string(),
                );
            };
            Ok(())
        }
        Pat::Cons(_) => todo!(),
        Pat::Tuple(_) => todo!(),
        Pat::Alias(_, _) => todo!(),
    }
}

// TODO: Reuse this function. Implemented after this patter has been used a few times.. (check source code!)
fn ctype_to_typeenv(t: &CType) -> TypeEnv {
    match t {
        CType::Base(b) => TypeEnv::Sigma(b.clone()),
        CType::New(n) => TypeEnv::Gamma(n.clone()),
        // TODO: Note that consume var is lost here! Should Delta be updated to contain var?
        CType::Consume(_, c) => TypeEnv::Delta(c.clone()),
    }
}
