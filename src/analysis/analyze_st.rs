// Check that session type matches the body

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{Atom, Clause, Expr, Exprs, FunHead, Lit, Var},
    st_parser::ast::SessionMode,
};

use super::compute_init_env::FunEnv;

use crate::st_parser::ast::SessionMode::NotST;

use crate::cerl_parser::ast::Exprs::Single;

// Proper "export" instead of all this () null return. Printing is not proper output.
pub fn analyze_module(m: HashMap<FunHead, FunEnv>) -> () {
    for (fun_head, fun_env) in m {
        print!("Analyzing {:?} ... ", fun_head);
        if fun_env.spec.is_some() && fun_env.session.is_some() && fun_env.body.is_some() {
            let env = init_var_env(
                fun_env.spec.unwrap(),
                fun_env.session.unwrap(),
                fun_env.body.clone().unwrap().args,
            );
            println!("init analyze env {:?}", env);
            chk_st_exprs(env, fun_env.body.unwrap().body);
            // TODO: To check return type
        } else {
            println!("could not analyze function. Functions must have a body, spec and session to be analyzed.")
        }
    }
}

#[derive(Debug, Clone)]
enum VarType {
    Base(String),
    ST(SessionMode),
}

fn init_var_env(
    spec: (Vec<String>, Vec<String>),
    session: Vec<SessionMode>,
    args: Vec<Var>,
) -> HashMap<Var, VarType> {
    // Initial types for variables
    let mut env: HashMap<Var, VarType> = HashMap::new();

    let (input, _) = spec;

    for (i, var) in args.iter().enumerate() {
        let this_st = session.get(i).unwrap();
        match this_st {
            NotST => {
                // If not session type use the type from -spec
                env.insert(
                    var.clone(),
                    VarType::Base(input.get(i).unwrap().to_string()),
                );
            }
            SessionMode::Fresh(_, _) => {
                // Check that -spec type matches (consistency)
                if input.get(i).unwrap().to_string() == "fresh".to_owned() {
                    // Get session type and insert
                    env.insert(var.clone(), VarType::ST(this_st.clone()));
                } else {
                    panic!("-session does not match -spec!! Issue is: Var {:?} is {:?} according to -spec, but should be fresh() to match -session: {:?}", var, input.get(i).unwrap(), this_st);
                };
            }
            SessionMode::Ongoing(_, _) => {
                // Check that -spec type matches (consistency)
                if input.get(i).unwrap().to_string() == "ongoing".to_owned() {
                    // Get session type and insert
                    env.insert(var.clone(), VarType::ST(this_st.clone()));
                } else {
                    panic!("-session does not match -spec!! Issue is: Var {:?} is {:?} according to -spec, but should be ongoing() to match -session: {:?}", var, input.get(i).unwrap(), this_st);
                };
            }
        }
    }

    env
}

// One one way forward: Dig into the body and see how it goes, one step at a time.

fn chk_st_exprs(env: HashMap<Var, VarType>, exprs: Exprs) -> HashMap<Var, VarType> {
    match exprs {
        Exprs::Single(expr) => chk_st_expr(env, *expr),
        Exprs::Values(_) => todo!(),
    }
}

fn chk_st_expr(env: HashMap<Var, VarType>, expr: Expr) -> HashMap<Var, VarType> {
    match expr {
        Expr::Var(_) => todo!("var"),
        Expr::Fname(_) => todo!("fname"),
        Expr::Lit(_) => todo!("lit"),
        Expr::Fun(_) => todo!("fun"),
        Expr::Cons(_) => todo!("cons"),
        Expr::Tuple(_) => todo!("tuple"),
        Expr::Let(_, _, _) => todo!("let"),
        Expr::Case(e, c) => {
            // Make a new env for every clause where the expr is matched to the var name in the clause in the env
            // Ignore when for now. TODO: Maybe consider to not ignore "when"
            let mut clause_env = Vec::new();
            for clause in &c {
                clause_env.push(chk_st_clause(env.clone(), e.clone(), clause.clone()));
            }
            // Note: Nothing comes "after" the case. Everything that needs to be checked are encapsulated within.
            // Or maybe. The condition for "passing" the st check comes after. "best" env should be passed?
            println!("\nOh case? | {:?} | {:?}", e, c);
            env
        }
        Expr::LetRec(_, _) => todo!("letrec"),
        Expr::Apply(_, _) => todo!("apply"),
        Expr::Call(mod_name, call_name, args) => {
            // Ignore everything except calls that includes sending messages
            if mod_name
                == Single(Box::new(Expr::Lit(Lit::Atom(Atom(
                    "gen_server_plus".to_owned(),
                )))))
                && call_name == Single(Box::new(Expr::Lit(Lit::Atom(Atom("call".to_owned())))))
            {
                panic!("YES!");
            }
            println!("\nOh call? {:?} {:?} {:?}", mod_name, call_name, args);
            env
        }
        Expr::PrimOp(_, _) => todo!("primop"),
        Expr::Receive(_, _, _) => todo!("receive"),
        Expr::Try(_, _, _, _, _) => todo!("try"),
        Expr::Do(e1, e2) => chk_st_exprs(chk_st_exprs(env, e1), e2),
        Expr::Catch(_) => todo!("catch"),
        Expr::Map(_, _) => todo!("map"),
    }
}

fn chk_st_clause(
    env: HashMap<Var, VarType>,
    exprs: Exprs,
    clause: Clause,
) -> HashMap<Var, VarType> {
    todo!();
    env
}
