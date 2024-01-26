// Check that session type matches the body

use std::collections::HashMap;

use crate::{
    analysis::{analyze_fun::get_bif_fun_type, analyze_st::try_st_env_update},
    cerl_parser::ast::{Atom, Clause, Expr, Exprs, FunName, Lit, Pat, Var},
    st_parser::ast::{SessionDef, SessionElement, SessionType, Types},
};

use super::compute_init_env::FunEnv;

use crate::st_parser::ast::SessionType::NotST;

// Proper "export" instead of all this () null return. Printing is not proper output.
pub fn analyze_module(m: &HashMap<FunName, FunEnv>) -> bool {
    let mut overall_acceptance = true; // Assume all is good until proven otherwise
    for (fun_head, fun_env) in m {
        print!("Analyzing {:?} ... ", fun_head);
        if fun_env.spec.is_some() && fun_env.session.is_some() && fun_env.body.is_some() {
            let var_env = init_var_env(
                fun_env.spec.as_ref().unwrap(),
                fun_env.session.as_ref().unwrap(),
                &fun_env.body.as_ref().unwrap().args,
            );
            print!("init analyze env {:?}", var_env);
            // TODO: Find a nice way to represent multiple possible cases
            let res_analysis =
                chk_st_exprs(m, var_env, &fun_env.body.as_ref().unwrap().body).unwrap();
            if res_analysis.len() < 1 {
                overall_acceptance = false;
                println!(" not OK, no result")
            }
            for (return_type, res_env) in res_analysis {
                print!(" res env is {:?}", res_env);
                // Check res env is acceptable
                let (_, spec_return_type) = fun_env.spec.as_ref().unwrap();
                let acceptable_res_env = validate_res_env(
                    &return_type,
                    &spec_return_type,
                    &fun_env.session.as_ref().unwrap().return_type,
                    &fun_env.session.as_ref().unwrap().binders,
                    res_env,
                );
                if !acceptable_res_env {
                    overall_acceptance = false;
                }
                println!(" checking env is acceptable: {:?}\n", acceptable_res_env);
                // TODO: To check return type
            }
        } else {
            // TODO: Should functions that cannot be analyzed result in an error? They do not right now.
            println!("could not analyze function. Functions must have a body, spec and session to be analyzed.")
        }
    }
    overall_acceptance
}

// TODO: I have a feeling that I should be able to refactor this type away
#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Base(Types),
    ST(SessionType),
}

// Bind variables to types before "evaluation"/Checking session type for concrete function.
fn init_var_env(
    spec: &(Vec<Types>, Types),
    session: &SessionDef,
    args: &Vec<Var>,
) -> HashMap<Var, VarType> {
    // Initial types for variables
    let mut env: HashMap<Var, VarType> = HashMap::new();

    let (input, _) = spec;

    // TODO: Check length of session.st and args matches!

    for (i, var) in args.iter().enumerate() {
        let this_st = session.st.get(i).unwrap();
        match this_st {
            NotST => {
                // If not session type use the type from -spec
                env.insert(var.clone(), VarType::Base(input.get(i).unwrap().clone()));
            }
            SessionType::New(_) => {
                // Check that -spec type matches (consistency)
                if *input.get(i).unwrap() == Types::Single("new".to_owned()) {
                    // Get session type and insert
                    env.insert(var.clone(), VarType::ST(this_st.clone()));
                } else {
                    panic!("-session does not match -spec!! Issue is: Var {:?} is {:?} according to -spec, but should be server() to match -session: {:?}", var, input.get(i).unwrap(), this_st);
                };
            }
            SessionType::Ongoing(_, _) => {
                // Check that -spec type matches (consistency)
                if *input.get(i).unwrap() == Types::Single("ongoing".to_owned()) {
                    // Get session type and insert
                    env.insert(var.clone(), VarType::ST(this_st.clone()));
                } else {
                    panic!("-session does not match -spec!! Issue is: Var {:?} is {:?} according to -spec, but should be session() to match -session: {:?}", var, input.get(i).unwrap(), this_st);
                };
            }
        }
    }

    env
}

// One one way forward: Dig into the body and see how it goes, one step at a time.

pub fn chk_st_exprs(
    m: &HashMap<FunName, FunEnv>,
    env: HashMap<Var, VarType>,
    exprs: &Exprs,
) -> Result<Vec<(VarType, HashMap<Var, VarType>)>, String> {
    let Exprs(exprs) = exprs;
    let mut res: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
    for elm in exprs {
        // TODO: What happens to the environment in values? is the environment update from value 1 kept until value 2 or are they all starting with the same base environment. I made this last assumption as it is easier.
        let mut res_tmp = chk_st_expr(m, env.clone(), elm)?;
        res.append(&mut res_tmp);
    }
    Ok(res)
}

fn chk_lit(l: &Lit) -> Types {
    match l {
        Lit::Int(_) => Types::Single("number".to_owned()), // TODO: enum types instead of string match?
        Lit::Float(_) => Types::Single("float".to_owned()), // TODO: enum types instead of string match?
        Lit::Atom(_) => Types::Single("atom".to_owned()), // TODO: enum types instead of string match?
        Lit::Char(_) => Types::Single("char".to_owned()), // TODO: enum types instead of string match?
        Lit::Cons(_) => todo!("Cons not yet supported. Main issue: Uniform list or not?"),
        Lit::Tuple(t) => {
            let mut tl = vec![];
            for elm in t {
                tl.push(chk_lit(elm));
            }
            Types::Tuple(tl)
        }
        Lit::String(_) => Types::Single("string".to_owned()), // TODO: enum types instead of string match?
        Lit::Nil => Types::Single("nil".to_owned()), // TODO: enum types instead of string match?
    }
}

fn chk_st_expr(
    m: &HashMap<FunName, FunEnv>,
    env: HashMap<Var, VarType>,
    expr: &Expr,
) -> Result<Vec<(VarType, HashMap<Var, VarType>)>, String> {
    match expr {
        Expr::Var(v) => {
            // TODO: Maybe not assume variable is defined?
            Ok(vec![(env.get(&v).unwrap().clone(), env)])
        }
        Expr::Fname(_) => todo!("What is the type of fname?"),
        Expr::Lit(l) => Ok(vec![(VarType::Base(chk_lit(l)), env)]),
        Expr::Fun(_) => todo!("fun"),
        Expr::Cons(_) => todo!("cons"),
        Expr::Tuple(_) => todo!("tuple"),
        Expr::Let(vars, e1, e2) => {
            // I'll consider let a special case of case with only one clause and without any when condition.
            // So, the work here is to rewrite let into a single case version of case

            // Convert vars to pats
            let mut vars_as_pats = vec![];
            for var in vars {
                vars_as_pats.push(Pat::Var(var.clone()));
            }

            // TODO: Dummy when
            let dummy_when = Exprs(vec![Expr::Lit(Lit::Atom(Atom("true".to_owned())))]);

            let let_in_clause = Clause {
                pats: vars_as_pats,
                when: dummy_when,
                res: e2.clone(),
            };

            let let_in_case = Expr::Case(e1.clone(), vec![let_in_clause]);

            chk_st_expr(m, env, &let_in_case)
        }
        Expr::Case(e, c) => {
            // Make a new env for every clause where the expr is matched to the var name in the clause in the env
            // Ignore when for now. TODO: Maybe consider to not ignore "when"
            let e_res = chk_st_exprs(m, env, e);
            if e_res.is_err() {
                {
                    println!("Eliminated possible env because {:?}", e_res);
                    ()
                }
                return Ok(vec![]);
            }
            let e_res = e_res.unwrap();

            let mut res: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
            // It is possible that e1 contains something that leads to more options.
            for (cur_return_type, cur_env) in e_res {
                // 1.  Common clause env update with e_res (for this case in e_res)
                // 1a. check overall type can even match: Top-level type must e able to match:
                //       Singleton vs cons vs tuple
                //                 if cons/tuple: Number of arguments must match
                // Assignment can be in a nested "data structure" as well, e.g. [{{A,B},C},D]
                // Arg + return type update env function needed.
                // The same thing needs to happen in the case of let v = e1 in e2

                // 1c.  Put new into context ot the concrete clause
                //            Ensure nothing is redefined here.

                // 2a. For each clause: Run inner expression

                for clause in c {
                    // Perform assignment to variables in clause
                    let clause_env = env_update_pattern_from_return_type(
                        cur_env.clone(),
                        clause.pats.clone(),
                        cur_return_type.clone(),
                    );

                    match clause_env {
                        Ok(clause_env) => {
                            // Ignore conditions, then when expression.
                            // TODO: Can the when condition consume the Session Type?

                            // Use clause env when checking the expression contained within the clause
                            // There may be multiple options when checking the expression contained within
                            let e_clause_res = chk_st_exprs(m, clause_env, &clause.res);
                            match e_clause_res {
                                Ok(mut e_clause_res) => res.append(&mut e_clause_res),
                                Err(e) => {
                                    println!("Eliminated possible env because {:?}", e);
                                    ()
                                }
                            }
                        }
                        Err(e) => {
                            println!("Eliminated possible env because {:?}", e);
                            ()
                        }
                    }
                }
                // Note: Nothing comes "after" the case. Everything that needs to be checked are encapsulated within.
                // Or maybe. The condition for "passing" the st check comes after. "best" env should be passed?
                //println!("\nOh case? | {:?} | {:?}", e, c);
            }
            Ok(res)
        }
        Expr::LetRec(_, _) => todo!("letrec"),
        Expr::Call(call, args) => {
            // Try to match as a BIF
            let try_bif = get_bif_fun_type(call);
            if try_bif.is_ok() {
                return Ok(vec![(VarType::Base(try_bif.unwrap()), env)]);
            };

            // Try to check if function is part of a session-type
            // Function: try_st_env_update
            let (try_st_type, try_st_env) = try_st_env_update(env, call, args)?;
            Ok(vec![(try_st_type, try_st_env)])
        }
        Expr::Receive(_, _, _) => todo!("receive"),
        Expr::Try(_, _, _, _, _) => todo!("try"),
        Expr::Do(e1, e2) => {
            let e1_res = chk_st_exprs(m, env, e1)?;
            let mut res: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
            // It is possible that e1 contains something that leads to more options.
            for elm in e1_res {
                let (_, elm_env) = elm;
                let mut e2_res = chk_st_exprs(m, elm_env, e2)?;
                res.append(&mut e2_res);
            }
            Ok(res)
        }
        Expr::Catch(_) => todo!("catch"),
        Expr::Map(_, _) => todo!("map"),
    }
}

// This function needs to be recursive, the naive approach should be OK.
// Remember that `_` may have different meaning in erlang and core erlang !
fn env_update_pattern_from_return_type(
    mut env: HashMap<Var, VarType>,
    pat: Vec<Pat>,
    value: VarType,
) -> Result<HashMap<Var, VarType>, String> {
    // 1a: If pattern only has one variable, this variable contains all - no pattern "matching" required
    if pat.len() == 1 {
        // Alright
        match pat.first().unwrap() {
            Pat::Var(v) => {
                // If Pat is 1 long and type then the type can be inserted directly
                match env.get(v) {
                    Some(v_old) => {
                        if *v_old != value {
                            // TODO: ST value change is expected. How to handle.
                            return Err("Change variable type is not yet supported".to_owned());
                        }
                    }
                    None => {
                        env.insert(v.clone(), value);
                    }
                }
                return Ok(env);
            }
            Pat::Lit(_) => {
                // If Pattern is a literal, we do not care, nothing to be checked
                return Ok(env);
            }
            Pat::Cons(c) => return env_update_pattern_from_return_type(env, c.clone(), value),
            Pat::Tuple(t) => return env_update_pattern_from_return_type(env, t.clone(), value),
            Pat::Alias(_, _) => todo!("alias not implemented"),
        }

        // If Pat = Var then we are a go
        // If Pat = Lit then what?
        // If Cons/Tuple ... well it did not work
        // If Alias= What?
    }

    // 1b: Outer length matches
    match value {
        VarType::Base(t) => match t {
            Types::Single(_) => {
                Err(format!("Mismatch between return type length and pattern length. Pattern has {:?} and type is {:?}", pat, t))
            }
            Types::Tuple(tuple) => {
                // Length matches, run for all
                let vt_zip = pat.iter().zip(tuple.iter());

                for (pat_elm, val_elm) in vt_zip {
                    env = env_update_pattern_from_return_type(
                        env,
                        vec![pat_elm.clone()],
                        VarType::Base(val_elm.clone()),
                    )?;
                }
                Ok(env)
            }
            Types::Cons(cons) => todo!("cons not yet implemented"),
        },
        VarType::ST(st) => {
            match st {
                NotST => todo!("NotST should maybe never be used?"),
                SessionType::New(content) => {
                    // TL;DR: It only works if pat has type "{VarName, 'ready'}"
                    // TODO: Maybe it is bad to hard-code ready into the analysis tool.... For later

                    if pat.len() != 2 {
                        return Err(format!("Wrong pattern length for session type return inside tuple"));
                    }
                    let Pat::Var(res_val_name) = pat.first().unwrap() else { return Err(format!("Wrong format return value session type"))};

                    // Variable name to bind is now known. Check it is usable
                    match env.get(&res_val_name) {
                        Some(_) => todo!(),
                        None => {
                            let mut env = env.clone();
                            env.insert(res_val_name.clone(), VarType::ST(SessionType::New(content)));
                            Ok(env)
                        },
                    }
                },
                SessionType::Ongoing(content, residual) => { // TODO: avoid duplicated case.....
                    // TL;DR: It only works if pat has type "{VarName, 'ready'}"
                    // TODO: Maybe it is bad to hard-code ready into the analysis tool.... For later

                    if pat.len() != 2 {
                        return Err(format!("Wrong pattern length for session type return inside tuple"));
                    }
                    let Pat::Var(res_val_name) = pat.first().unwrap() else { return Err(format!("Wrong format return value session type"))};

                    // Variable name to bind is now known. Check it is usable
                    match env.get(&res_val_name) {
                        Some(_) => todo!(),
                        None => {
                            let mut env = env.clone();
                            env.insert(res_val_name.clone(), VarType::ST(SessionType::Ongoing(content, residual)));
                            Ok(env)
                        },
                    }
                },
            }
        },
    }
}

fn validate_res_env(
    st_spec_return_type: &VarType,
    base_spec_return_type: &Types,
    env_return_type: &Vec<SessionElement>,
    session: &HashMap<Var, Vec<SessionElement>>,
    env: HashMap<Var, VarType>,
) -> bool {
    // Validate return type match
    match st_spec_return_type {
        VarType::Base(bt) => {
            if *bt != *base_spec_return_type {
                println!("Mismatch between expected return type and -spec");
                return false;
            }
        }
        VarType::ST(env_return_type) => {
            let mut env_return_type = match env_return_type {
                NotST => todo!("Not ST"),
                SessionType::New(st) => st,
                SessionType::Ongoing(st, _) => st,
            };
            // TODO: Better way to compare vectors
            for (elm1, elm2) in env_return_type.iter().zip(env_return_type.iter()) {
                if elm1 != elm2 {
                    println!("Return type mismatch.");
                    return false;
                }
            }
        }
    }

    // Validate binders for session type variables in env
    for (key, elm) in env {
        match elm {
            VarType::Base(_) => (),
            VarType::ST(st) => {
                match st {
                    NotST => todo!(),
                    SessionType::Ongoing(st_cnt, local_res_binder) => {
                        // TODO: Maybe expect "end." ?
                        match session.get(&key) {
                            Some(val) => {
                                if *val != st_cnt {
                                    println!(
                                        "Var {:?} is {:?} but should be {:?} according to binder.",
                                        key, st_cnt, *val
                                    );
                                    return false;
                                }
                                if local_res_binder.is_some() {
                                    println!("Local binder and var binder is contradictory. Unacceptable.");
                                    return false;
                                }
                            }
                            None => match local_res_binder {
                                Some(local_res_binder) => {
                                    if local_res_binder != st_cnt {
                                        println!("Local binder session type check does not match: Expected {:?} but found {:?}. Validation failed.", local_res_binder, st_cnt);
                                        return false;
                                    }
                                }
                                None => {
                                    println!(
                                        "Var {:?} does not have a binder, cannot accept env.",
                                        key
                                    );
                                    return false;
                                }
                            },
                        }
                        // if st_cnt.len() != 0 {
                        //     println!("Session type not consumed!");
                        //     return false;
                        // }
                    }
                    SessionType::New(_) => {
                        println!("Session type not constructed!");
                        return false;
                    }
                }
            }
        }
    }
    true
}
