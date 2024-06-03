use crate::{
    cerl_parser::ast::{Atom, Lit, Pat, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CType},
        types::{BaseType, Label, SessionType, SessionTypesList},
    },
};

use super::{
    env::{TypeEnv, TypeEnvs},
    session::diff_consumed,
};

// TODO: Get rid of all "panic!"

// Maybe it would be better to return Result instead of panic .
pub fn expr(module: &CModule, envs: &mut TypeEnvs, e: &CExpr) -> Result<CType, String> {
    match e {
        CExpr::Var(v) => e_base(envs, v),
        CExpr::Lit(l) => Ok(e_lit(l)),
        CExpr::Cons(_) => todo!(),
        CExpr::Tuple(_) => todo!(),
        CExpr::Let(v, e1, e2) => e_let(module, envs, v, e1, e2),
        CExpr::Case(base_expr, clauses) => e_case(module, envs, &base_expr, clauses),
        CExpr::Call(call, args) => e_call(module, envs, call, args),
        CExpr::Do(e1, e2) => e_do(module, envs, e1, e2),
    }
}

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

    let gsp_sync_send =
        CFunCall::CCall(Atom("gen_server_plus".to_owned()), Atom("call".to_owned()));
    let gsp_new = CFunCall::CCall(Atom("gen_server_plus".to_owned()), Atom("new".to_owned()));

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
        let CType::CBaseType(sending_val) =
            (match expr(module, &mut TypeEnvs(envs.0.clone()), sending_expr) {
                Ok(ok_val) => ok_val,
                Err(err_val) => {
                    return Err(format!("e_call gsp_sync_send failed because {}", err_val))
                }
            })
        else {
            return Err(format!("e_call gsp_sync_send can only send base values"));
        };
        //println!("{:?}", envs);

        // Get current session
        let session_id = &args[1];
        let CExpr::Var(session_var) = session_id else {
            return Err(format!(
                "e_call gsp_sync_send Session variable name must be used, not expression"
            ));
        };
        let CType::CConsumeType(_, session_type) =
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
            return Err(format!(
                "e_call gsp_sync_send second argument must be session type"
            ));
        };
        let mut session_type = session_type;

        // TODO: session_var must match, if not defined, set it!

        // Session must have at least one element otherwise it is not possible to continue:
        if session_type.0.len() < 1 {
            return Err(format!(
                "e_call gsp_sync_send Cannot send on empty/consumed session"
            ));
        }

        // It can be either an atom matching a label, or a simple value sent. Let us check:
        match session_type.0.first().unwrap() {
            SessionType::Send(to_send_val) => {
                if session_type.0.len() < 2 {
                    panic!("Session type too short for sync send-receive")
                }
                // Send base value
                if sending_val != *to_send_val {
                    panic!("Mismatch between expected to send and actual type.")
                }
                session_type.0.remove(0);
                // Return type is received value
                let SessionType::Receive(received) = session_type.0.remove(0) else {
                    return Err(format!("Expects ping-pong send-receive"));
                };
                envs.0
                    .insert(session_var.clone(), TypeEnv::Delta(session_type));
                return Ok(CType::CBaseType(received));
            }
            SessionType::Receive(_) => panic!("Session type says receive, we are about to send"),
            SessionType::MakeChoice(_, _) => {
                panic!("Session type MakeChoice, expected OfferChoice")
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
                        return Ok(CType::CConsumeType(
                            Some(session_var.clone()),
                            continuation.clone(),
                        ));
                    }
                    None => panic!("Trying to make choice not offered by session"),
                }
            }
            SessionType::End => panic!("Session type is End, but we are about to use it"),
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
            panic!(
                "gen_server_plus:new only works with one argument. {:?}",
                args
            )
        }
        // Get the third argument. This is the important value
        let server_pid = args.first().unwrap();

        // TODO Call by value isolation!!!!! Important!!!
        // println!("TODO Call by value isolation!!!!! Important!!!");
        let CType::CNewType(session_type) =
            (match expr(module, &mut TypeEnvs(envs.0.clone()), server_pid) {
                Ok(ok_val) => ok_val,
                Err(err_val) => return Err(format!("E_call gsp_new failed due to {}", err_val)),
            })
        else {
            return Err(format!("Must construct new session here"));
        };
        // println!("type of pid {:?}", session_type);

        return Ok(CType::CConsumeType(None, session_type));

        // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
        // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
        // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
        // Generic "check env before-after" is strongly needed now!

        // We can only send base values

        // If it is not a base-value we assume it is select
    }

    // TODO: More clever way to handle BIF
    let bif_io_format = CFunCall::CCall(Atom("io".to_owned()), Atom("format".to_owned()));
    if *call == bif_io_format {
        return Ok(CType::CBaseType(BaseType::Atom(Atom("ok".to_string()))));
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
        TypeEnv::Gamma(n) => Ok(CType::CNewType(n.clone())),
        TypeEnv::Delta(c) => Ok(CType::CConsumeType(None, c.clone())),
        TypeEnv::Sigma(res) => Ok(CType::CBaseType(res.clone())),
    }
}

fn e_lit(l: &Lit) -> CType {
    match l {
        Lit::Int(_) => CType::CBaseType(BaseType::Integer),
        Lit::Float(_) => CType::CBaseType(BaseType::Float),
        Lit::Atom(a) => CType::CBaseType(BaseType::Atom(a.clone())),
        Lit::Char(_) => todo!(),
        Lit::Cons(_) => todo!(),
        Lit::Tuple(_) => todo!(),
        Lit::String(_) => todo!(),
    }
}

fn e_case(
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
    let base_res = expr(&module, &mut case_base_expr_envs, base_expr);
    if base_res.is_err() {
        return Err(format!(
            "E_case failed in base case because {}",
            base_res.err().unwrap()
        ));
    }

    // base expr must return a consume session type, otherwise the choices cannot be checked against a session type
    // In other words: Any base or new type is a type error
    let CType::CConsumeType(var, to_consume) = base_res.unwrap() else {
        return Err(format!("Type error case base expr must be consume"));
    };

    // Var must be defined
    let Some(var) = var else {
        return Err(format!("Cannot work without binding session"));
    };

    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    if clauses.len() == 0 {
        // TODO: Actually remember. I decided that no clauses == Do
        //       Tbh this seemed like a simplification, but properly isn't, include Do in Contract Core Erlang?
        return Err(format!("Zero clauses in case makes no sense."));
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

fn must_st_consume_expr(
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
            let diff_ok = diff_consumed(before_envs, &current_envs);
            if diff_ok.is_err() {
                return  Err(format!("must st consume expr {:?} failed because {}\n DEBUG:\nBefore Envs: {:?}\nAfter Envs: {:?}", e, diff_ok.err().unwrap(), before_envs, current_envs));
            }

            // Return return_type
            Ok(return_type)
        }
        Err(err_val) => return Err(format!("must_st_consume_expr failed because {}", err_val)),
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
                panic!("Lit on left hand side can only be unified with lit on right hand side")
            };
            // NOOP if they are equal, but panic if they are not
            if *l != *e_lit {
                return Err(format!(
                    "pattern matching failed in Lit must be equal in pattern matching."
                ));
            };
            Ok(())
        }
        Pat::Cons(_) => todo!(),
        Pat::Tuple(t) => {
            todo!()
        }
        Pat::Alias(_, _) => todo!(),
    }
}

// TODO: Reuse this function. Implemented after this patter has been used a few times.. (check source code!)
fn ctype_to_typeenv(t: &CType) -> TypeEnv {
    match t {
        CType::CBaseType(b) => TypeEnv::Sigma(b.clone()),
        CType::CNewType(n) => TypeEnv::Gamma(n.clone()),
        // TODO: Note that consume var is lost here! Should Delta be updated to contain var?
        CType::CConsumeType(_, c) => TypeEnv::Delta(c.clone()),
    }
}

fn lookup_st_from_label(
    pat: &Vec<Pat>,
    session_offers: &SessionTypesList,
) -> Result<SessionTypesList, String> {
    // We expect at very specific structure here:
    // The pattern must be one element long with just a atom
    // The session type must be a offer-type and one of the labels must be the pattern atom
    // We assume that the caller has checked the session_types list
    // If any of the above does not hold, we have a type error

    // Get the label for current pattern
    if pat.len() != 1 {
        return Err(format!("Label cannot exist if length is not 1"));
    }
    let Pat::Lit(atom) = pat.first().unwrap() else {
        return Err(format!("label must be an atom #1"));
    };
    let Lit::Atom(atom) = atom else {
        return Err(format!("label must be an atom #2"));
    };
    let crate::cerl_parser::ast::Atom(pat_label) = atom;

    // Look for label in session type
    if session_offers.0.len() != 1 {
        return Err(format!("Length of session offers is not as expected"));
    }
    let SessionType::OfferChoice(session_offers) = session_offers.0.first().unwrap() else {
        return Err(format!("Case not possible without session offer"));
    };

    // TODO: Can clone be avoided here?
    match session_offers.get(&Label(pat_label.clone())) {
        Some(st) => Ok(st.clone()),
        None => return Err(format!("No matching offer found in session!")),
    }
}
