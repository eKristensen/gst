use std::rc::Rc;

use crate::{
    cerl_parser::ast::{Atom, FunName, Lit, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CModule, CPat, CType},
        types::{BaseType, ChoiceType, SessionType, SessionTypesList},
    },
    type_checker::{
        fun::bif_fun,
        session::{gsp_close, gsp_new, gsp_sync_send},
    },
};

use super::{
    env::{CastEnv, TypeEnv, TypeEnvs},
    fun::e_app,
    session::{e_case_offer, must_st_consume_expr},
};

pub fn expr(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    e: &CExpr,
) -> Result<CType, String> {
    match e {
        CExpr::Var(_, v) => e_base(envs, v),
        CExpr::Lit(_, l) => match (**l).clone() {
            // TODO: Deduplicate. Avoid repeating cpat_to_ctype function here
            Lit::Atom(atom_val) => match atom_val.0.as_str() {
                "true" => Ok(CType::Base(BaseType::Boolean)),
                "false" => Ok(CType::Base(BaseType::Boolean)),
                _ => Ok(CType::Base(BaseType::Atom(atom_val.clone()))),
            },
            l => Ok(e_lit(&l)),
        },
        CExpr::Cons(_, cons) => match expr_base_type_list(module, envs, cast_env, cons) {
            Ok(res) => Ok(CType::Base(BaseType::Cons(res))),
            Err(err_val) => Err(format!("expr cons failed because {}", err_val)),
        },
        CExpr::Tuple(_, tuple) => match expr_base_type_list(module, envs, cast_env, tuple) {
            Ok(res) => Ok(CType::Base(BaseType::Tuple(res))),
            Err(err_val) => Err(format!("expr tuple failed because {}", err_val)),
        },
        CExpr::Let(_, v, e1, e2) => e_let(module, envs, cast_env, v, e1, e2),
        CExpr::Case(_, base_expr, clauses) => e_case(module, envs, cast_env, base_expr, clauses),
        CExpr::PrimOp(_, _, _) => todo!("PrimOp not supported yet."),
        CExpr::Apply(_, call_name, args) => e_apply(module, envs, cast_env, call_name, args),
        CExpr::Call(_, call_module, call_name, args) => {
            e_call(module, envs, cast_env, call_module, call_name, args)
        }
        CExpr::Do(_, e1, e2) => e_do(module, envs, cast_env, e1, e2),
        CExpr::Fun(_, args, body) => e_fun(module, envs, cast_env, args, body),
        CExpr::ApplyFun(_, fun_name_var, args) => {
            e_app_fun(module, envs, cast_env, fun_name_var, args)
        }
    }
}

fn expr_base_type_list(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    e: &Vec<CExpr>,
) -> Result<Vec<BaseType>, String> {
    let mut res: Vec<BaseType> = Vec::new();
    for elm in e {
        // Try to evaluate in isolated environment
        match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, elm) {
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

fn e_apply(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    call_name: &Rc<FunName>,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    if call_name.arity != args.len() {
        return Err("Inconsistent Apply".to_string());
    };

    // Type as if it was a call to the same module avoid duplication of type checker code
    e_call(module, envs, cast_env, &module.name, &call_name.name, args)
}

// TODO: e-call function is too big, split into smaller parts.
fn e_call(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    call_module: &Rc<Atom>,
    call_name: &Rc<Atom>,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    // Call can be both a send operation and a "normal" call.
    // We need to check and perform the right kind of type check
    // 1) A GS+ call
    // 1a) Send base value  (e_send)
    // 1b) Send make choice (e_select)
    // 2)  A native call    (e_app, the fallback)

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
        ("gen_server_plus", "new", [server_pid], None) => {
            gsp_new(module, envs, cast_env, server_pid)
        }
        ("gen_server_plus", "new", _, _) => Err("Invalid gs+:new call".to_string()),
        ("gen_server_plus", "call", [_, session_id, sending_expr], None) => {
            println!("TODO: First and second argument are not checked right now. Should they?");
            gsp_sync_send(module, envs, cast_env, session_id, sending_expr)
        }
        ("gen_server_plus", "close", [_, session_id], None) => {
            println!("TODO: First argument is not checked right now. Should it?");
            gsp_close(module, envs, cast_env, session_id)
        }
        (call_module, call_name, args, None) => {
            bif_fun(module, envs, cast_env, call_module, call_name, args)
        }
        (_, _, args, Some(fun_clauses)) => e_app(module, envs, cast_env, args, fun_clauses),
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

fn e_fun(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    args: &Vec<Var>,
    body: &CExpr,
) -> Result<CType, String> {
    // Can I somehow reuse all the function typing I've done so far?
    // The general idea is the same, or almost the same. We have some vars that give the input types. With these types check the body, and see what the return type is by type checking the body
    // Answer: Irrelevant as the fun check is prep + expr check on body, i.e. there is nothing to copy.
    // 1) Ensure all vars has a type otherwise assume dynamic
    // NOTE: Env isolation is required. We enter an isolated scobe that we need to typecheck.
    let before_envs: TypeEnvs = TypeEnvs(envs.0.clone());
    let mut current_envs: TypeEnvs = TypeEnvs(envs.0.clone());
    let mut fun_ctype_in: Vec<BaseType> = Vec::new();
    for arg in args {
        if !current_envs.0.contains_key(arg) {
            // TODO: Arg clone and then into... There must be a better way.
            current_envs
                .0
                .insert((arg.clone()).into(), TypeEnv::Sigma(BaseType::Dynamic));
            fun_ctype_in.push(BaseType::Dynamic)
        } else {
            todo!("Did not expect any bound variables here");
        }
    }
    // 2) Typecheck body
    // 3) Return CType is the last expr - No, the CType is the input to output function type.
    match must_st_consume_expr(module, &before_envs, &mut current_envs, cast_env, body) {
        Ok(CType::Base(ok_val)) => Ok(CType::Base(BaseType::Fun(
            Some(fun_ctype_in),
            Some(ok_val.into()),
        ))),
        Ok(_) => Err(format!(
            "Current it is assumed that anonymous functions can only output base types."
        )),
        Err(err_val) => Err(format!("e_fun failed because {}", err_val)),
    }
}

fn e_app_fun(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    fun_name_var: &Rc<Var>,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    // Check contract: Check args CType
    match envs.0.get(fun_name_var) {
        Some(fun_ctype) => {
            match fun_ctype {
                TypeEnv::Sigma(BaseType::Fun(fun_in,fun_out )) => {
                    match (fun_in, fun_out) {
                        (Some(fun_in), Some(fun_out)) => {
                            todo!("Alright let me se those fun in and out types: {:?} {:?}", fun_in, fun_out)
                        },
                        _ => todo!("What to do when input/output types of an anonymous function is not defined?")
                    }
                }
                _ => Err("Applying anonymous function is only supported if var name refers to a funciton base type".to_owned()),
            }
        }
        None => return Err("Applying undefined anonymous function is not supported.".to_owned()),
    }
    // Insert casts if needed (Will it ever be needed?)
    // Based on fun_name_var we get the output type
    // We should not check the function body again. That is what e_fun is for.
}

fn e_do(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    e1: &CExpr,
    e2: &CExpr,
) -> Result<CType, String> {
    // TODO: e_do is the only reason we have cPat::Any
    // _ is not properly supported in cerl, and I assume a CType of Base::Any It should work fine
    // right?
    e_let(module, envs, cast_env, &CPat::Any, e1, e2)
}

// TODO: Reuse this function for : E_case, E_app ("call-by-value")
// Evaluate type in an isolated environment and ensure there are no lose ends
// Almost like expr function with an additional environment check
// Models call-by-value in Erlang. Isolated evaluation, i.e environment is used as base, but not changed.
fn e_let(
    module: &CModule,
    envs: &mut TypeEnvs,
    cast_env: &mut CastEnv,
    v: &CPat,
    e1: &CExpr,
    e2: &CExpr,
) -> Result<CType, String> {
    // Clone env to compare consume later. Keep using original ref otherwise changes within are going to be lost
    //let envs_copy_baseline = TypeEnvs(envs.0.clone());
    let pat_ok = pattern_matching(module, envs, cast_env, v, e1);
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

    match must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, e2) {
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
    cast_env: &mut CastEnv,
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
    let base_res =
        must_st_consume_expr(module, envs, &mut case_base_expr_envs, cast_env, base_expr);
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
        if let SessionType::Choice(ChoiceType::Make, offers) = st.0.first().unwrap() {
            let case_res = e_case_offer(module, envs, cast_env, offers, clauses)?;
            // TODO: Write down typing system "assumption"
            // When we are here in the code, it means at least one offer has matched
            // Therefore we should update the session for the var and remove consume, pop the first
            // element
            // TODO: I feel like I should have the var name already somewhere
            match base_expr {
                CExpr::Var(_, v) => {
                    envs.0
                        .insert(v.clone(), TypeEnv::Delta(SessionTypesList(vec![])));
                }
                _ => todo!("Not supported should never happen"),
            };
            return Ok(case_res);
        }
    } // send/receive or anything else
      // If not, then treat as normal
    e_case_branching(module, envs, cast_env, base_expr, clauses) // Base and new are "normal".

    // Remember common return type for each clause, this common type is the return value.
}

// "Normal" branching
fn e_case_branching(
    module: &CModule,
    envs: &TypeEnvs,
    cast_env: &mut CastEnv,
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
            cast_env,
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
    cast_env: &mut CastEnv,
    v: &CPat,
    e: &CExpr,
) -> Result<(), String> {
    // Compare by abstraction, type
    let e_ctype = must_st_consume_expr(module, &TypeEnvs(envs.0.clone()), envs, cast_env, e)?;
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
        if let CPat::Var(_, v) = v {
            envs.0.insert(v.clone(), ctype_to_typeenv(&e_ctype));
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
        CPat::Lit(_, lit) => match (**lit).clone() {
            Lit::Atom(atom_val) => match atom_val.0.as_str() {
                "true" => CType::Base(BaseType::Boolean),
                "false" => CType::Base(BaseType::Boolean),
                _ => CType::Base(BaseType::Atom(atom_val.clone())),
            },
            l => e_lit(&l),
        },
        CPat::Var(_, v) => {
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
        CPat::Cons(_, _c) => todo!(),
        CPat::Tuple(_, _t) => todo!(),
        CPat::Alias(_, _, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use crate::cerl_parser::ast::{Atom, CLoc, Loc};

    use super::*;

    #[test]
    fn sanity_check_scope_test_e_do_let() {
        // To ensure scope isolation actually works and does not break by accident
        let module = CModule {
            name: Atom("".to_owned()).into(),
            mspec: None,
            functions: HashMap::new(),
            fallback_args: HashMap::new(),
        };
        let mut envs = TypeEnvs(HashMap::new());
        let e1 = CExpr::Let(
            CLoc {
                comment: None,
                start: Loc { line: 0, column: 0 },
                end: Loc { line: 0, column: 0 },
            }
            .into(),
            CPat::Var(
                CLoc {
                    comment: None,
                    start: Loc { line: 0, column: 0 },
                    end: Loc { line: 0, column: 0 },
                }
                .into(),
                Var("X".to_owned()).into(),
            )
            .into(),
            CExpr::Call(
                CLoc {
                    comment: None,
                    start: Loc { line: 0, column: 0 },
                    end: Loc { line: 0, column: 0 },
                }
                .into(),
                Atom("erlang".to_owned()).into(),
                Atom("+".to_owned()).into(),
                vec![
                    CExpr::Lit(
                        CLoc {
                            comment: None,
                            start: Loc { line: 0, column: 0 },
                            end: Loc { line: 0, column: 0 },
                        }
                        .into(),
                        Lit::Int(1).into(),
                    ),
                    CExpr::Lit(
                        CLoc {
                            comment: None,
                            start: Loc { line: 0, column: 0 },
                            end: Loc { line: 0, column: 0 },
                        }
                        .into(),
                        Lit::Int(2).into(),
                    ),
                ],
            )
            .into(),
            CExpr::Let(
                CLoc {
                    comment: None,
                    start: Loc { line: 0, column: 0 },
                    end: Loc { line: 0, column: 0 },
                }
                .into(),
                CPat::Var(
                    CLoc {
                        comment: None,
                        start: Loc { line: 0, column: 0 },
                        end: Loc { line: 0, column: 0 },
                    }
                    .into(),
                    Var("Y".to_owned()).into(),
                )
                .into(),
                CExpr::Call(
                    CLoc {
                        comment: None,
                        start: Loc { line: 0, column: 0 },
                        end: Loc { line: 0, column: 0 },
                    }
                    .into(),
                    Atom("erlang".to_owned()).into(),
                    Atom("+".to_owned()).into(),
                    vec![
                        CExpr::Var(
                            CLoc {
                                comment: None,
                                start: Loc { line: 0, column: 0 },
                                end: Loc { line: 0, column: 0 },
                            }
                            .into(),
                            Var("X".to_owned()).into(),
                        ),
                        CExpr::Lit(
                            CLoc {
                                comment: None,
                                start: Loc { line: 0, column: 0 },
                                end: Loc { line: 0, column: 0 },
                            }
                            .into(),
                            Lit::Int(3).into(),
                        ),
                    ],
                )
                .into(),
                CExpr::Call(
                    CLoc {
                        comment: None,
                        start: Loc { line: 0, column: 0 },
                        end: Loc { line: 0, column: 0 },
                    }
                    .into(),
                    Atom("erlang".to_owned()).into(),
                    Atom("+".to_owned()).into(),
                    vec![
                        CExpr::Var(
                            CLoc {
                                comment: None,
                                start: Loc { line: 0, column: 0 },
                                end: Loc { line: 0, column: 0 },
                            }
                            .into(),
                            Var("X".to_owned()).into(),
                        ),
                        CExpr::Var(
                            CLoc {
                                comment: None,
                                start: Loc { line: 0, column: 0 },
                                end: Loc { line: 0, column: 0 },
                            }
                            .into(),
                            Var("Y".to_owned()).into(),
                        ),
                    ],
                )
                .into(),
            )
            .into(),
        );
        let e2 = CExpr::Var(
            CLoc {
                comment: None,
                start: Loc { line: 0, column: 0 },
                end: Loc { line: 0, column: 0 },
            }
            .into(),
            Var("Y".to_owned()).into(),
        );
        assert!(e_do(&module, &mut envs, &mut CastEnv(HashMap::new()), &e1, &e2).is_err())
    }
}
