use crate::{
    cerl_parser::ast::{FunName, Lit, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CPat, CType},
        types::{BaseType, SessionType, SessionTypesList},
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
    println!(
        "DEBUG: START EXPR CHECK: Env var names are {:?} before {:?}",
        envs.0.keys(),
        e
    );
    match e {
        CExpr::Var(v) => e_base(envs, v),
        CExpr::Lit(l) => match l {
            Lit::Atom(atom_val) => match atom_val.0.as_str() {
                "true" => Ok(CType::Base(BaseType::Boolean)),
                "false" => Ok(CType::Base(BaseType::Boolean)),
                _ => Ok(CType::Base(BaseType::Atom(atom_val.clone()))),
            },
            l => Ok(e_lit(l)),
        },
        CExpr::Cons(cons) => match expr_base_type_list(module, envs, cons) {
            Ok(res) => Ok(CType::Base(BaseType::Cons(res))),
            Err(err_val) => Err(format!("expr cons failed because {}", err_val)),
        },
        CExpr::Tuple(tuple) => match expr_base_type_list(module, envs, tuple) {
            Ok(res) => Ok(CType::Base(BaseType::Tuple(res))),
            Err(err_val) => Err(format!("expr tuple failed because {}", err_val)),
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

    // New concept kinda like:
    // 1) normalize call name
    // 2) Lookup in defined functions
    // 3) match on normalized name (module and call), arg count and true/false lookup result

    // Normalize call name
    let (call_module, call_name) = match call {
        CFunCall::PrimOp(_) => todo!("PrimOp not supported yet."),
        CFunCall::Apply(name) if name.arity == args.len() => (&module.name, &name.name),
        CFunCall::Call(call_module_name, name) => (call_module_name, name),
        _ => panic!("Inconsistent call."),
    };

    // TODO: Improvement: Inter-module contract lookup
    // Lookup function in module
    let known_call = module.functions.get(&FunName {
        name: call_name.clone(),
        arity: args.len(),
    });

    // TODO: Very imporant assumption: There will never be a name conflict between build in
    // functions and user defined functions. Maybe it is better to check rather than assume.

    match (
        call_module.0.as_str(),
        call_name.0.as_str(),
        args.as_slice(),
        known_call,
    ) {
        ("gen_server_plus", "new", [server_pid], None) => return gsp_new(module, envs, server_pid),
        ("gen_server_plus", "new", _, _) => return Err("Invalid gs+:new call".to_string()),
        ("gen_server_plus", "call", [_, session_id, sending_expr], None) => {
            println!("TODO: First and second argument are not checked right now. Should they?");
            return gsp_sync_send(module, envs, session_id, sending_expr);
        }
        ("gen_server_plus", "call", [_, session_id, sending_expr], None) => {
            return Err("Invalid gs+:call call".to_string())
        }
        (call_module, call_name, args, None) => {
            return bif_fun(module, envs, call_module, call_name, &args)
        }
        (_, _, args, Some(fun_clauses)) => return e_app(module, envs, args, fun_clauses),
        _ => panic!("Invalid call. Issue unknown."),
    }
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
    // TODO: e_do is the only reason we have cPat::Any
    // _ is not properly supported in cerl, and I assume a CType of Base::Any It should work fine
    // right?
    e_let(module, envs, &CPat::Any, e1, e2)
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
    //let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let pat_ok = pattern_matching(module, envs, v, e1);
    if let Err(err_val) = pat_ok {
        return Err(format!("e_let failed #1 because {}", err_val));
    }

    // TODO: IMPORTANT I THINK I FOUND IT!
    // No substituion/ pattern is NOT saved for e2 !!!!!
    // E.g.
    // Let V1 = E1 in (let V2 = E2 in E3)
    // Then, V1 is not defined in (let V2 = E2 in E3) !!!
    // WUT? Too much isolation here ????
    // OR NOT? It makes sense that V1 is not defined before the let expr for V1...
    // NO! It is in E1 that V1 is not defined, not the continuation !

    println!(
        "\n!!!! DEBUG:\ne1 is: {:?}\ne2 is {:?}\n Env \n{:?}",
        e1,
        e2,
        envs.0.keys()
    );
    println!("must_st_consume_expr called by e_let");
    match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, e2) {
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
    envs: &mut TypeEnvs,
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
        if let SessionType::MakeChoice(offers) = st.0.first().unwrap() {
            let case_res = e_case_offer(module, envs, offers, clauses)?;
            // TODO: Write down typing system "assumption"
            // When we are here in the code, it means at least one offer has matched
            // Therefore we should update the session for the var and remove consume, pop the first
            // element
            // TODO: I feel like I should have the var name already somewhere
            match base_expr {
                CExpr::Var(v) => {
                    envs.0
                        .insert(v.clone(), TypeEnv::Delta(SessionTypesList(vec![])));
                }
                _ => todo!("Not supported should never happen"),
            };
            return Ok(case_res);
        }
    } // send/receive or anything else
      // If not, then treat as normal
    e_case_branching(module, envs, base_expr, clauses) // Base and new are "normal".

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

    for clause in clauses {
        // Current let in implementation is sufficient for each clause:
        // 1) A base expr
        // 2) With a binder (pattern)
        // 3) and a continuation
        let mut clause_envs = TypeEnvs(envs.0.clone());
        if clause.pats.len() != 1 {
            return Err(("e_case_branching: Expected top Pat to be one long.").to_string());
        }
        let clause_res = e_let(
            module,
            &mut clause_envs,
            clause.pats.first().unwrap(),
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
    // Compare by abstraction, type
    let e_ctype = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, e)?;
    let v_ctype = cpat_to_ctype(envs, v);
    let mut acceptable = false;
    if e_ctype == v_ctype {
        acceptable = true;
    }
    // Manual subtyping
    if v_ctype == CType::Base(BaseType::Any) {
        acceptable = true;
    }
    if acceptable {
        match v {
            CPat::Var(v) => {
                envs.0.insert(v.clone(), ctype_to_typeenv(&e_ctype));
            }
            _ => (),
        };
        return Ok(());
    }
    Err(format!(
        "Pattern mismatch. Pat is {:?} and cexpr is {:?} ",
        v_ctype, e_ctype
    ))
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

fn cpat_to_ctype(envs: &TypeEnvs, p: &CPat) -> CType {
    match p {
        CPat::Lit(lit) => match lit {
            Lit::Atom(atom_val) => match atom_val.0.as_str() {
                "true" => CType::Base(BaseType::Boolean),
                "false" => CType::Base(BaseType::Boolean),
                _ => CType::Base(BaseType::Atom(atom_val.clone())),
            },
            l => e_lit(l),
        },
        CPat::Var(v) => {
            match envs.0.get(v) {
                Some(v) => match v {
                    TypeEnv::Gamma(v) => CType::New(v.clone()),
                    TypeEnv::Delta(v) => CType::Consume(v.clone()),
                    TypeEnv::Sigma(v) => CType::Base(v.clone()),
                },
                None => CType::Base(BaseType::Any), // If var is not found, allow binding the value with "any"
            }
        }
        CPat::Any => CType::Base(BaseType::Any),
        CPat::Cons(_c) => todo!(),
        CPat::Tuple(_t) => todo!(),
        CPat::Alias(_, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {

    use std::{collections::HashMap, rc::Rc};

    use crate::cerl_parser::ast::Atom;

    use super::*;

    #[test]
    fn sanity_check_scope_test_e_do_let() {
        // To ensure scope isolation actually works and does not break by accident
        let module = CModule {
            name: Atom("".to_owned()),
            mspec: None,
            functions: HashMap::new(),
            fallback_args: HashMap::new(),
        };
        let mut envs = TypeEnvs(HashMap::new());
        let e1 = CExpr::Let(
            CPat::Var(Var("X".to_owned())),
            Rc::new(CExpr::Call(
                CFunCall::Call(Atom("erlang".to_owned()), Atom("+".to_owned())),
                vec![CExpr::Lit(Lit::Int(1)), CExpr::Lit(Lit::Int(2))],
            )),
            Rc::new(CExpr::Let(
                CPat::Var(Var("Y".to_owned())),
                Rc::new(CExpr::Call(
                    CFunCall::Call(Atom("erlang".to_owned()), Atom("+".to_owned())),
                    vec![CExpr::Var(Var("X".to_owned())), CExpr::Lit(Lit::Int(3))],
                )),
                Rc::new(CExpr::Call(
                    CFunCall::Call(Atom("erlang".to_owned()), Atom("+".to_owned())),
                    vec![
                        CExpr::Var(Var("X".to_owned())),
                        CExpr::Var(Var("Y".to_owned())),
                    ],
                )),
            )),
        );
        let e2 = CExpr::Var(Var("Y".to_owned()));
        assert!(e_do(&module, &mut envs, &e1, &e2).is_err())
    }
}
