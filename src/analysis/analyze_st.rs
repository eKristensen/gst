// Check that session type matches the body

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{Expr, Exprs, FunHead, Var},
    st_parser::ast::SessionMode,
};

use super::compute_init_env::FunEnv;

use crate::st_parser::ast::SessionMode::NotST;

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

#[derive(Debug)]
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
            SessionMode::Fresh(_) => {
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
        Expr::Var(_) => todo!(),
        Expr::Fname(_) => todo!(),
        Expr::Lit(_) => todo!(),
        Expr::Fun(_) => todo!(),
        Expr::Cons(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Let(_, _, _) => todo!(),
        Expr::Case(_, _) => todo!(),
        Expr::LetRec(_, _) => todo!(),
        Expr::Apply(_, _) => todo!(),
        Expr::Call(_, _, _) => todo!(),
        Expr::PrimOp(_, _) => todo!(),
        Expr::Receive(_, _, _) => todo!(),
        Expr::Try(_, _, _, _, _) => todo!(),
        Expr::Do(_, _) => todo!(),
        Expr::Catch(_) => todo!(),
        Expr::Map(_, _) => todo!(),
    }
}
