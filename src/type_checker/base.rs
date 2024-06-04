use crate::{
    cerl_parser::ast::{Lit, Pat, Var},
    contract_cerl::{
        ast::{CExpr, CFunCall, CModule, CType},
        types::BaseType,
    },
    type_checker::{
        fun::bif_fun,
        session::{gsp_new, gsp_sync_send},
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
        CExpr::Cons(cons) => match expr_base_type_list(module, envs, cons) {
            Ok(res) => Ok(CType::Base(BaseType::Cons(res))),
            Err(err_val) => return Err(format!("expr cons failed because {}", err_val)),
        },
        CExpr::Tuple(tuple) => match expr_base_type_list(module, envs, tuple) {
            Ok(res) => Ok(CType::Base(BaseType::Tuple(res))),
            Err(err_val) => return Err(format!("expr tuple failed because {}", err_val)),
        },
        CExpr::Let(v, e1, e2) => e_let(module, envs, v, e1, e2),
        CExpr::Case(base_expr, clauses) => e_case(module, envs, base_expr, clauses),
        CExpr::Call(call, args) => e_call(module, envs, call, args),
        CExpr::Do(e1, e2) => e_do(module, envs, e1, e2),
    }
}

fn expr_base_type_list(
    module: &CModule,
    envs: &mut TypeEnvs,
    e: &Vec<CExpr>,
) -> Result<Vec<BaseType>, String> {
    let mut res: Vec<BaseType> = Vec::new();
    for elm in e {
        // Try to evaluate in isolated environment
        match must_st_consume_expr(module, &mut TypeEnvs(envs.0.clone()), envs, elm) {
            Ok(ok_val) => {
                let CType::Base(ok_val) = ok_val else {
                    return Err("Only base types supported in cons or tuple".to_string());
                };
                res.push(ok_val)
            }
            Err(err_val) => {
                return Err(format!(
                    "Evaluation for type of element in Cons or Tuple failed because of {}",
                    err_val
                ))
            }
        }
    }
    Ok(res)
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

    // Try session constructor
    let gsp_new_res = gsp_new(module, envs, call, args);
    if let Ok(res_ok) = gsp_new_res {
        return Ok(res_ok);
    }

    // Try send-receive
    let gsp_sync_send_res = gsp_sync_send(module, envs, call, args);
    if let Ok(res_ok) = gsp_sync_send_res {
        return Ok(res_ok);
    }

    // Try build-in functions (bif)
    let bif_res = bif_fun(call);
    if let Ok(res_ok) = bif_res {
        return Ok(res_ok);
    }

    Err(format!(
        "Could not type call {:?}. gsp+ new error: {}, gsp+ send error {}, bif error {}",
        call,
        gsp_new_res.err().unwrap(),
        gsp_sync_send_res.err().unwrap(),
        bif_res.err().unwrap()
    ))
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
    CType::Base(e_lit_aux(l))
}

fn e_lit_aux(l: &Lit) -> BaseType {
    match l {
        Lit::Int(_) => BaseType::Integer,
        Lit::Float(_) => BaseType::Float,
        Lit::Atom(a) => BaseType::Atom(a.clone()),
        Lit::Char(_) => BaseType::Char,
        Lit::Cons(cons) => BaseType::Cons(cons.iter().map(e_lit_aux).collect()),
        Lit::Tuple(tuple) => BaseType::Tuple(tuple.iter().map(e_lit_aux).collect()),
        Lit::String(_) => BaseType::String,
    }
}

fn e_do(module: &CModule, envs: &mut TypeEnvs, e1: &CExpr, e2: &CExpr) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let eval_ok = expr(module, envs, e1);
    if let Err(err_val) = eval_ok {
        return Err(format!("e_do failed because {}", err_val));
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
    if let Err(err_val) = pat_ok {
        return Err(format!("e_let failed #1 because {}", err_val));
    }
    let env_debug_copy = TypeEnvs(envs.0.clone());

    // println!("LET must consume");
    match must_st_consume_expr(module, &envs_copy_baseline, envs, e2) {
        Ok(ok_val) => Ok(ok_val),
        Err(err_val) => Err(format!("e_let failed #2 because {}", err_val)),
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
            // Get expression type
            let t = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, e);
            if let Err(err_val) = t {
                return Err(format!(
                    "pattern matching failed in var because {}",
                    err_val
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
        Pat::Cons(l_cons) => {
            // What this means: Both sides must have cons, and content must be compatible
            let CExpr::Cons(e) = e else {
                return Err("Cons pattern mismatch".to_string());
            };
            for (v_elm, e_elm) in l_cons.iter().zip(e.iter()) {
                if let Err(err_val) = pattern_matching(module, envs, v_elm, e_elm) {
                    return Err(format!("Pat cons failed due to {}", err_val));
                }
            }
            Ok(())
        }
        Pat::Tuple(l_tuple) => {
            // What this means: Both sides must have tuple, and content must be compatible
            let CExpr::Tuple(e) = e else {
                return Err("Tuple pattern mismatch".to_string());
            };
            for (v_elm, e_elm) in l_tuple.iter().zip(e.iter()) {
                if let Err(err_val) = pattern_matching(module, envs, v_elm, e_elm) {
                    return Err(format!("Pat tuple failed due to {}", err_val));
                }
            }
            Ok(())
        }
        Pat::Alias(_, _) => todo!(
            "How to pattern match alias? Find example where this todo is triggered and implement."
        ),
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
