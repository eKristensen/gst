use crate::{
    cerl_parser::ast::{Lit, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CPat, CType},
        types::{BaseType, SessionType},
    },
    type_checker::{
        fun::bif_fun,
        session::{gsp_new, gsp_sync_send},
    },
};

use super::{
    env::{TypeEnv, TypeEnvs},
    fun::e_app,
    session::{e_case_offer, must_st_consume_expr},
};

pub fn expr(module: &CModule, envs: &mut TypeEnvs, e: &CExpr) -> Result<CType, String> {
    match e {
        CExpr::Var(v) => e_base(envs, v),
        CExpr::Lit(l) => Ok(e_lit(l)),
        CExpr::Cons(cons) => match expr_base_type_list(module, envs, cons) {
            Ok(res) => Ok(CType::Base(BaseType::Cons(res))),
            Err(err_val) => Err(format!("expr cons failed because {}", err_val)),
        },
        CExpr::Tuple(tuple) => match expr_base_type_list(module, envs, tuple) {
            Ok(res) => Ok(CType::Base(BaseType::Tuple(res))),
            Err(err_val) => Err(format!("expr tuple failed because {}", err_val)),
        },
        CExpr::Let(v, e1, e2) => {
            let mut cpat_v: Vec<CPat> = Vec::new(); // TODO: This conversion should be moved
            for elm in v {
                cpat_v.push(CPat::Var(elm.clone()));
            }
            let v = CPat::Tuple(cpat_v);
            e_let(module, envs, &v, e1, e2)
        }
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
        match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, elm) {
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

    // Try function application
    let app_res = e_app(module, envs, call, args);
    if let Ok(res_ok) = app_res {
        return Ok(res_ok);
    }

    Err(format!(
        "Could not type call {:?}. gsp+ new error: {}, gsp+ send error {}, bif error {}, app error {}",
        call,
        gsp_new_res.err().unwrap(),
        gsp_sync_send_res.err().unwrap(),
        bif_res.err().unwrap(),
        app_res.err().unwrap(),
    ))
}

fn e_base(envs: &TypeEnvs, v: &Var) -> Result<CType, String> {
    let res = envs.0.get(v);
    if res.is_none() {
        return Err(format!("used var that was not defined {}", v));
    }
    match res.unwrap() {
        // TODO: How to allow both base value checked properly, and not to "return" a base type?
        TypeEnv::Gamma(n) => Ok(CType::New(n.clone())),
        TypeEnv::Delta(c) => Ok(CType::Consume(c.clone())),
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
        Lit::Nil => BaseType::Cons(vec![]),
    }
}

fn e_do(module: &CModule, envs: &mut TypeEnvs, e1: &CExpr, e2: &CExpr) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let eval_ok = expr(module, envs, e1);
    if let Err(err_val) = eval_ok {
        return Err(format!("e_do failed because {}", err_val));
    }

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
    v: &CPat,
    e1: &CExpr,
    e2: &CExpr,
) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let pat_ok = pattern_matching(module, envs, &v, e1);
    if let Err(err_val) = pat_ok {
        return Err(format!("e_let failed #1 because {}", err_val));
    }

    match must_st_consume_expr(module, &envs_copy_baseline, envs, e2) {
        Ok(ok_val) => Ok(ok_val),
        Err(err_val) => Err(format!("e_let failed #2 because {}", err_val)),
    }
}

// TODO: Cases should be more generic:
// Flow should be
// 1) If base_expr return type is a consume type:
//    If it also is a select type, then the case block should be treated as offer for the session type
// Otherwise: Normal case function where each pattern is matched against the base_expr return type
// Always: Require common return type.
// Same isolation as used until now should work just fine.
// Result: No need for var name in consume return type (CType) and support for ordinary branching.
fn e_case(
    module: &CModule,
    envs: &TypeEnvs,
    base_expr: &CExpr,
    clauses: &Vec<CClause>,
) -> Result<CType, String> {
    if clauses.is_empty() {
        // TODO: Actually remember. I decided that no clauses == Do
        //       Tbh this seemed like a simplification, but properly isn't, include Do in Contract Core Erlang?
        return Err("Zero clauses in case makes no sense.".to_string());
    }

    // Copy the env to save as a base case, we do not want to change the source env in this rule. The unchanged env must be returned.
    // Maybe a better way to types env clone?
    // Base expr type check
    // TODO: A bit manual env clone, maybe fix?
    let mut case_base_expr_envs = TypeEnvs(envs.0.clone());
    let base_res = must_st_consume_expr(module, envs, &mut case_base_expr_envs, base_expr);
    let Ok(base_res) = base_res else {
        return Err(format!(
            "E_case failed in base case because {}",
            base_res.err().unwrap()
        ));
    };

    // Check base_expr type to find the correct way to type this case expression
    if let CType::Consume(st) = base_res.clone() {
        // So many quesitons:
        // - Can we consume nothing, i.e. can st be "end." or empty?
        // - Can we send/receive here? Would it even make sense at all ? No then it is just a
        // basic let in again
        // - Can we select something here? No, again it would just be a basic let in again
        // - Can we offer something here? Yes, that is the whole point.
        if let SessionType::OfferChoice(offers) = st.0.first().unwrap() {
            return e_case_offer(module, envs, offers, clauses);
        }
    } // send/receive or anything else
      // If not, then treat as normal
    e_case_branching(module, envs, &base_expr, clauses) // Base and new are "normal".

    // Remember common return type for each clause, this common type is the return value.
}

// "Normal" branching
fn e_case_branching(
    module: &CModule,
    envs: &TypeEnvs,
    base_expr: &CExpr,
    clauses: &Vec<CClause>,
) -> Result<CType, String> {
    // Normal Case
    // It does not matter what base_expr for the case is, the only thing that matters is what type
    // it does return, i.e. the CType. We use that for each clause to check.
    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    let mut case_start_envs = TypeEnvs(envs.0.clone());

    for clause in clauses {
        // Current let in implementation is sufficient for each clause:
        // 1) A base expr
        // 2) With a binder (pattern)
        // 3) and a continuation
        let mut clause_envs = TypeEnvs(envs.0.clone());
        if clause.pats.len() != 1 {
            return Err(format!(
                "e_case_branching: Expected top Pat to be one long."
            ));
        }
        let clause_res = e_let(
            module,
            &mut clause_envs,
            &clause.pats.first().unwrap(),
            base_expr,
            &clause.res,
        );
        if clause_res.is_err() {
            return Err(format!(
                "Case clause failed because {}",
                clause_res.err().unwrap()
            ));
        }

        common_return_type.push(clause_res.clone().unwrap());
    }

    // Check return-type is the same.
    let common_return_type_base = common_return_type.first().unwrap();
    common_return_type
        .iter()
        .all(|item| *item == *common_return_type_base);

    Ok(common_return_type_base.clone())
}

// Aka: unification
// https://en.wikipedia.org/wiki/Unification_(computer_science)#Unification_algorithms
fn pattern_matching(
    module: &CModule,
    envs: &mut TypeEnvs,
    v: &CPat,
    e: &CExpr,
) -> Result<(), String> {
    match v {
        CPat::Var(v) => {
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
        CPat::Lit(l) => {
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
        CPat::Cons(l_cons) => {
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
        CPat::Tuple(l_tuple) => {
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
        CPat::Alias(_, _) => todo!(
            "How to pattern match alias? Find example where this todo is triggered and implement."
        ),
    }
}

// TODO: Reuse this function. Implemented after this patter has been used a few times.. (check source code!)
// Function purpose: Takes a CType and add it to the right environment.
fn ctype_to_typeenv(t: &CType) -> TypeEnv {
    match t {
        CType::Base(b) => TypeEnv::Sigma(b.clone()),
        CType::New(n) => TypeEnv::Gamma(n.clone()),
        // TODO: Note that consume var is lost here! Should Delta be updated to contain var?
        CType::Consume(c) => TypeEnv::Delta(c.clone()),
    }
}
