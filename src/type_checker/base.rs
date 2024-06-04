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
        CExpr::Cons(cons) => {
            let res = expr_base_type_list(module, envs, cons)?;
            Ok(CType::Base(BaseType::Cons(res)))
        }
        CExpr::Tuple(tuple) => {
            let res = expr_base_type_list(module, envs, tuple)?;
            Ok(CType::Base(BaseType::Tuple(res)))
        }
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
        match must_st_consume_expr(module, envs, &mut TypeEnvs(envs.0.clone()), elm) {
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
    if let Ok(res_ok) = gsp_new(module, envs, call, args) {
        return Ok(res_ok);
    }

    // Try send-receive
    if let Ok(res_ok) = gsp_sync_send(module, envs, call, args) {
        return Ok(res_ok);
    }

    // Try build-in functions (bif)
    if let Ok(res_ok) = bif_fun(call) {
        return Ok(res_ok);
    }

    Err("Could not type call".to_string())
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
