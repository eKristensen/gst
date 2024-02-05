// Check that session type matches the body

use std::{collections::HashMap, fmt};

use crate::{
    analysis::{analyze_fun::get_bif_fun_type, analyze_st::try_st_env_update},
    cerl_parser::ast::{Atom, Clause, Expr, Exprs, FunName, Lit, Pat, Var},
    st_parser::ast::{SessionDef, SessionElement, SessionType, Types},
};

use super::{
    analyze_fun::get_user_fun_type, analyze_st::extract_var_type, compute_init_env::FunEnv,
};

use crate::st_parser::ast::SessionType::NotST;

// Proper "export" instead of all this () null return. Printing is not proper output.
pub fn analyze_module(m: &HashMap<FunName, FunEnv>) -> bool {
    let mut overall_acceptance = true; // Assume all is good until proven otherwise
    for (fun_head, fun_env) in m {
        print!("Analyzing {} ... ", fun_head);
        if fun_env.spec.is_some() && fun_env.session.is_some() && fun_env.body.is_some() {
            let (var_env, new_binders) = init_var_env(
                fun_env.spec.as_ref().unwrap(),
                fun_env.session.as_ref().unwrap(),
                &fun_env.body.as_ref().unwrap().args,
                &fun_env.session.as_ref().unwrap().binders,
            );
            print!("\ninit analyze env:");
            for (key, val) in &var_env {
                print!("\n{} = {}", key, val);
            }
            // TODO: Find a nice way to represent multiple possible cases
            let res_analysis =
                chk_st_exprs(m, &var_env, &fun_env.body.as_ref().unwrap().body).unwrap();
            println!("\n\nAnalyzed all possible environments, checking each of them (if any)");
            if res_analysis.is_empty() {
                overall_acceptance = false;
                println!("\nNOT OK, no possible executions that match spec according to analysis.")
            }
            for (return_type, res_env) in res_analysis {
                print!("\nPossible env:");
                for (key, val) in &res_env {
                    print!("\n{} = {}", key, val);
                }
                // Check res env is acceptable
                let (_, spec_return_type) = fun_env.spec.as_ref().unwrap();
                let acceptable_res_env =
                    validate_res_env(&return_type, spec_return_type, &new_binders, res_env);
                if !acceptable_res_env {
                    overall_acceptance = false;
                }
                println!("\nChecking env is acceptable: {:?}\n", acceptable_res_env);
                // TODO: To check return type
            }
        } else {
            // TODO: Should functions that cannot be analyzed result in an error? They do not right now.
            println!("could not analyze function. Functions must have a body, spec and session to be analyzed.")
        }
        println!("--------------------------------------------------------");
    }
    println!("Overall Acceptance: {}", overall_acceptance);
    overall_acceptance
}

// TODO: I have a feeling that I should be able to refactor this type away
#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Base(Types),
    ST(SessionType),
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarType::Base(res) => write!(f, "{}", res),
            VarType::ST(res) => write!(f, "{}", res),
        }
    }
}

// Bind variables to types before "evaluation"/Checking session type for concrete function.
fn init_var_env(
    spec: &(Vec<Types>, Types),
    session: &SessionDef,
    args: &[Var],
    binders: &HashMap<Var, Vec<SessionElement>>,
) -> (HashMap<Var, VarType>, HashMap<Var, Vec<SessionElement>>) // Returns: env, binders
{
    // Initial types for variables
    let mut env: HashMap<Var, VarType> = HashMap::new();
    let mut binders = binders.clone(); // TODO: Inefficient!

    let (input, _) = spec;

    // TODO: Check length of session.st and args matches!

    for (i, var) in args.iter().enumerate() {
        let this_st = session.st.get(i).unwrap();

        let fun_head_arg_type = extract_var_type(this_st, input.get(i).unwrap());

        env.insert(var.clone(), fun_head_arg_type.clone());

        if let VarType::ST(SessionType::Ongoing(_, Some(fun_head_rt_add_binder))) =
            fun_head_arg_type
        {
            binders.insert(var.clone(), fun_head_rt_add_binder);
        };
    }

    //println!("DEBUG: Binders created are: {:?}", binders);
    (env, binders)
}

// One one way forward: Dig into the body and see how it goes, one step at a time.

pub fn chk_st_exprs(
    m: &HashMap<FunName, FunEnv>,
    env: &HashMap<Var, VarType>,
    exprs: &Exprs,
) -> Result<Vec<(VarType, HashMap<Var, VarType>)>, String> {
    let Exprs(exprs) = exprs;
    let mut res: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
    for elm in exprs {
        // TODO: What happens to the environment in values? is the environment update from value 1 kept until value 2 or are they all starting with the same base environment. I made this last assumption as it is easier.
        let mut res_tmp = chk_st_expr(m, env, elm)?;
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
    env: &HashMap<Var, VarType>,
    expr: &Expr,
) -> Result<Vec<(VarType, HashMap<Var, VarType>)>, String> {
    match expr {
        Expr::Var(v) => {
            // TODO: Maybe not assume variable is defined?
            //println!("DEBUG: Lookup var {:?} in {:?}", v, env);
            Ok(vec![(env.get(v).unwrap().clone(), env.clone())])
        }
        Expr::Fname(_) => todo!("What is the type of fname?"),
        Expr::Lit(l) => Ok(vec![(VarType::Base(chk_lit(l)), env.clone())]),
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
                    println!("\nEliminated possible env because {:?}", e_res);
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
                            let e_clause_res = chk_st_exprs(m, &clause_env, &clause.res);
                            match e_clause_res {
                                Ok(mut e_clause_res) => res.append(&mut e_clause_res),
                                Err(e) => {
                                    println!("\n\nEliminated possible env because {:?}", e);
                                }
                            }
                        }
                        Err(e) => {
                            println!("\n\nEliminated possible env because {:?}", e);
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
                return Ok(vec![(VarType::Base(try_bif.unwrap()), env.clone())]);
            };

            // Try user-defined function which might include session types
            // UPDATE: Errors must be passed along, if there are no matches then return empty vector
            // Right now error messages are lost due to this stupid chain method I got here.
            let try_user_fun = get_user_fun_type(m, env, call, args);
            if try_user_fun.is_ok() {
                return try_user_fun;
            }
            //println!("DEBUG Failed user fun check {:?}", try_user_fun);

            // Try to check if function is part of a session-type
            // Function: try_st_env_update
            let try_st_env_update = try_st_env_update(env, call, args);
            if try_st_env_update.is_ok() {
                let (try_st_type, try_st_env) = try_st_env_update.unwrap();
                return Ok(vec![(try_st_type, try_st_env)]);
            }

            // TODO: Better way to collect errors
            let mut combined_err = "".to_string();
            if let Err(bif_err) = try_bif {
                combined_err.push_str(" Error from try bif: ");
                combined_err.push_str(&bif_err);
            }
            if let Err(usr_err) = try_user_fun {
                combined_err.push_str(" Error from try user fun: ");
                combined_err.push_str(&usr_err);
            }
            if let Err(env_err) = try_st_env_update {
                combined_err.push_str(" Error from try st env update: ");
                combined_err.push_str(&env_err);
            }

            return Err(combined_err);
        }
        Expr::Receive(_, _, _) => todo!("receive"),
        Expr::Try(_, _, _, _, _) => todo!("try"),
        Expr::Do(e1, e2) => {
            let e1_res = chk_st_exprs(m, env, e1)?;
            let mut res: Vec<(VarType, HashMap<Var, VarType>)> = vec![];
            // It is possible that e1 contains something that leads to more options.
            for elm in e1_res {
                let (_, elm_env) = elm;
                let mut e2_res = chk_st_exprs(m, &elm_env, e2)?;
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
pub fn env_update_pattern_from_return_type(
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
                            //return Err(format!("Change variable type is not yet supported {:?} {:?}", v_old, value));
                            env.insert(v.clone(), value);
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
            //Types::Cons(_) => todo!("cons not yet implemented"),
        },
        VarType::ST(st) => {
            match st {
                NotST => todo!("NotST should maybe never be used?"),
                SessionType::New(content) => {
                    // TL;DR: It only works if pat has type "{VarName, 'ready'}"
                    // TODO: Maybe it is bad to hard-code ready into the analysis tool.... For later

                    if pat.len() != 2 {
                        return Err("Wrong pattern length for session type return inside tuple".to_string());
                    }
                    let Pat::Var(res_val_name) = pat.first().unwrap() else { return Err("Wrong format return value session type".to_string())};

                    // Variable name to bind is now known. Check it is usable
                    match env.get(res_val_name) {
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
                        return Err("Wrong pattern length for session type return inside tuple".to_string());
                    }
                    let Pat::Var(res_val_name) = pat.first().unwrap() else { return Err("Wrong format return value session type".to_string())};

                    // Variable name to bind is now known. Check it is usable
                    match env.get(res_val_name) {
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
    session: &HashMap<Var, Vec<SessionElement>>,
    env: HashMap<Var, VarType>,
) -> bool {
    // Validate return type match
    match st_spec_return_type {
        VarType::Base(bt) => {
            if *bt != *base_spec_return_type {
                println!("\nEnvironment validation failed because: Mismatch between expected return type and -spec");
                return false;
            }
        }
        VarType::ST(env_return_type) => {
            let env_return_type = match env_return_type {
                NotST => todo!("Not ST"),
                SessionType::New(st) => st,
                SessionType::Ongoing(st, _) => st,
            };
            // TODO: Better way to compare vectors
            for (elm1, elm2) in env_return_type.iter().zip(env_return_type.iter()) {
                if elm1 != elm2 {
                    println!("\nEnvironment validation failed because Return type mismatch.");
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
                    SessionType::Ongoing(mut st_cnt, local_res_binder) => {
                        // TODO: Maybe expect "end." ?
                        // I'll choose to merge the two branches of ongoing for end check
                        // It means a returned type of ongoing(!string. -> end.) would match binder _1=[!string. end.]
                        // TODO: Ask Marco about the above
                        // Flattening of ongoing
                        if local_res_binder.is_some() {
                            let mut local_res_binder = local_res_binder.clone().unwrap();
                            st_cnt.append(&mut local_res_binder);
                        }
                        match session.get(&key) {
                            Some(val) => {
                                if *val != st_cnt {
                                    println!(
                                        "\nEnvironment validation failed because Var {:?} is {:?} but should be {:?} according to binder.",
                                        key, st_cnt, *val
                                    );
                                    return false;
                                }
                                // if local_res_binder.is_some() {
                                //     println!("Local binder and var binder is contradictory. Unacceptable.");
                                //     return false;
                                // }
                            }
                            None => match local_res_binder {
                                Some(local_res_binder) => {
                                    if local_res_binder != st_cnt {
                                        println!("\nEnvironment validation failed because Local binder session type check for {:?} does not match: Expected {:?} but found {:?}. Validation failed.", key, st_cnt, local_res_binder);
                                        return false;
                                    }
                                }
                                None => {
                                    println!(
                                        "\nEnvironment validation failed because Var {:?} does not have a binder, cannot accept env.",
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
                        // println!("Session type not constructed!");
                        // return false;
                    }
                }
            }
        }
    }
    true
}
