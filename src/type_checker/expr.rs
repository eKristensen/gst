use std::collections::HashSet;

use crate::{
    cerl_parser::ast::{Atom, Lit, Pat, Var},
    contract_cerl::{
        ast::{CClause, CExpr, CFunCall, CModule, CType},
        types::{BaseType, Label, SessionType, SessionTypesList},
    },
};

use super::env::{TypeEnv, TypeEnvs};

// Maybe it would be better to return Result instead of panic .
pub fn expr(module: &CModule, envs: &TypeEnvs, e: &CExpr) -> CType {
    match e {
        CExpr::Var(v) => e_base(envs, v),
        CExpr::Lit(l) => e_lit(l),
        CExpr::Cons(_) => todo!(),
        CExpr::Tuple(_) => todo!(),
        CExpr::Case(base_expr, clauses) => e_case(module, envs, &base_expr, clauses),
        CExpr::Call(call, args) => e_call(module, envs, call, args),
    }
}

fn e_call(module: &CModule, envs: &TypeEnvs, call: &CFunCall, args: &Vec<CExpr>) -> CType {
    // Call can be both a send operation and a "normal" call.
    // We need to check and perform the right kind of type check
    // 1) A GS+ call
    // 1a) Send base value  (e_send)
    // 1b) Send make choice (e_select)
    // 2)  A native call    (e_app, the fallback)

    let gen_server_plus =
        CFunCall::CCall(Atom("gen_server_plus".to_owned()), Atom("call".to_owned()));

    if *call == gen_server_plus {
        // To find the right sub-call we need to check the args. There must be three args
        if args.len() != 3 {
            panic!("gen_server_plus:call only works with three arguments.")
        }
        // Get the third argument. This is the important value
        let arg_to_check = &args[2];

        // Call by value, We need to argument type. Execution environment? Should I consider it isolated? I suppose?
        // The safest and more reasonable way to deal with the call-by-value is to assume it is like a let x (var-name) = expr type
        // And to assume the environment of the call-by-value is enclosed, i.e. we require all lose ends to be finished and do not save any expressions that may be defined while evaluating the value in call-by-value.
        // Generic "check env before-after" is strongly needed now!

        // We can only send base values

        // If it is not a base-value we assume it is select
    }
    todo!()
}

fn e_base(envs: &TypeEnvs, v: &Var) -> CType {
    let res = envs.0.get(v);
    if res.is_none() {
        panic!("used var that was not defined {:?}", v)
    }
    match res.unwrap() {
        TypeEnv::Gamma(_) => panic!("base value must be in Sigma"),
        TypeEnv::Delta(_) => panic!("base value must be in Sigma"),
        TypeEnv::Sigma(res) => CType::CBaseType(res.clone()),
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

fn e_case(module: &CModule, envs: &TypeEnvs, base_expr: &CExpr, clauses: &Vec<CClause>) -> CType {
    // Copy the env to save as a base case, we do not want to change the source env in this rule. The unchanged env must be returned.
    // Maybe a better way to types env clone?
    let case_start_envs = envs.0.clone();

    // Base line to check for new consumed sessions
    let base_vars: HashSet<Var> = case_start_envs.keys().cloned().collect();

    // Base expr type check
    // TODO: A bit manual env clone, maybe fix?
    let case_base_expr_envs = TypeEnvs(envs.0.clone());
    let base_res = expr(&module, &case_base_expr_envs, base_expr);

    // base expr must return a consume session type, otherwise the choices cannot be checked against a session type
    // In other words: Any base or new type is a type error
    let CType::CConsumeType(var, to_consume) = base_res else {
        panic!("Type error case base expr must be consume")
    };

    // Var must be defined
    let Some(var) = var else {
        panic!("Cannot work without binding session")
    };

    // Return types
    // TODO: Find a way to compare it without needing to save all return values
    let mut common_return_type: Vec<CType> = Vec::new();

    if clauses.len() == 0 {
        // TODO: Actually remember. I decided that no clauses == Do
        //       Tbh this seemed like a simplification, but properly isn't, include Do in Contract Core Erlang?
        panic!("Zero clauses in case makes no sense.")
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

        // Clone and update env for continuation.
        let mut clause_env = TypeEnvs(case_base_expr_envs.0.clone());
        clause_env
            .0
            .insert(var.clone(), TypeEnv::Delta(matching_st));

        // Type check clause expr
        let clause_res = expr(&module, &clause_env, &clause.res);

        // Add return type to check later
        common_return_type.push(clause_res.clone());

        // Check all that should be consumed, has been consumed (aka "finished" function)
        // Two stage: Find all to check, and then to check their value.
        let clause_vars: HashSet<Var> = clause_env.0.keys().cloned().collect();
        let must_be_consumed: HashSet<&Var> = clause_vars.difference(&base_vars).collect();
        for check_var in must_be_consumed {
            match clause_env.0.get(check_var).unwrap() {
                TypeEnv::Gamma(_) => continue,
                TypeEnv::Delta(check_consumed) => {
                    // Check session is consumed
                    if check_consumed.0.len() == 0 {
                        continue;
                    }
                    if check_consumed.0.len() == 1 {
                        if *check_consumed.0.first().unwrap() == SessionType::End {
                            continue;
                        }
                        panic!("Not consumed")
                    }
                    panic!("Not consumed")
                }
                TypeEnv::Sigma(_) => continue,
            }
        }
    }

    // Check return-type is the same.
    let common_return_type_base = common_return_type.first().unwrap();
    common_return_type
        .iter()
        .all(|item| *item == *common_return_type_base);

    // Remember common return type for each clause, this common type is the return value.

    todo!()
}

// Evaluate type in an isolated environment and ensure there are no lose ends
// Almost like expr function with an additional environment check
fn call_by_value(module: &CModule, envs: &TypeEnvs, e: &CExpr) -> CType {
    // Move the logic from "e_case" here
    // Include do and let in contract erlang
    // Use call-by-value in function call to get the type in a proper way.
    todo!()
}

fn lookup_st_from_label(pat: &Vec<Pat>, session_offers: &SessionTypesList) -> SessionTypesList {
    // We expect at very specific structure here:
    // The pattern must be one element long with just a atom
    // The session type must be a offer-type and one of the labels must be the pattern atom
    // We assume that the caller has checked the session_types list
    // If any of the above does not hold, we have a type error

    // Get the label for current pattern
    if pat.len() != 1 {
        panic!("Label cannot exist if length is not 1")
    }
    let Pat::Lit(atom) = pat.first().unwrap() else {
        panic!("label must be an atom #1")
    };
    let Lit::Atom(atom) = atom else {
        panic!("label must be an atom #2")
    };
    let crate::cerl_parser::ast::Atom(pat_label) = atom;

    // Look for label in session type
    if session_offers.0.len() != 1 {
        panic!("Length of session offers is not as expected");
    }
    let SessionType::OfferChoice(session_offers) = session_offers.0.first().unwrap() else {
        panic!("Case not possible without session offer")
    };

    // TODO: Can clone be avoided here?
    match session_offers.get(&Label(pat_label.clone())) {
        Some(st) => st.clone(),
        None => panic!("No matching offer found in session!"),
    }
}
