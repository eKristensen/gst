use std::collections::HashMap;

use crate::analysis::analyze_expr::chk_st_exprs;

use super::env::{FunEnv, Funcs, TypeEnv};


// Proper "export" error message instead binary true/false return.
pub fn analyze_module_functions(funcs: &Funcs) -> bool {
    let mut overall_acceptance = true; // Assume all is good until proven otherwise
    for (fun_head, fun_env) in funcs {
        print!("Analyzing {} ... ", fun_head);
        if fun_env.must_analyze() {


            // TODO: Replace by new "init_type_env" function
            let (type_env, new_binders) = init_type_env(
                fun_env.spec.as_ref().unwrap(),
                fun_env.session.as_ref().unwrap(),
                &fun_env.body.as_ref().unwrap().args,
                &fun_env.session.as_ref().unwrap().binders,
            );


            print!("\ninit analyze env:");
            for (key, val) in &type_env {
                print!("\n{} = {}", key, val);
            }
            // TODO: Find a nice way to represent multiple possible cases
            let res_analysis =
                chk_st_exprs(funcs, &type_env, &fun_env.body.as_ref().unwrap().body).unwrap();
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
            println!(" SKIP ANALYSIS. {}.", fun_env.comment)
        }
        println!("--------------------------------------------------------");
    }
    println!("Overall Acceptance: {}", overall_acceptance);
    overall_acceptance
}

// Bind variables to types before "evaluation"/Checking session type for concrete function.
fn init_type_env(
    fun_env: &FunEnv,
) -> TypeEnv // Returns: env, binders
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


fn validate_res_env(
    st_spec_return_type: &VarType,
    base_spec_return_type: &Types,
    session: &HashMap<Var, SessionElementList>,
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
            let SessionElementList(env_return_type) = match env_return_type {
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
                    SessionType::Ongoing(SessionElementList(mut st_cnt), local_res_binder) => {
                        // TODO: Maybe expect "end." ?
                        // I'll choose to merge the two branches of ongoing for end check
                        // It means a returned type of ongoing(!string. -> end.) would match binder _1=[!string. end.]
                        // TODO: Ask Marco about the above
                        // Flattening of ongoing
                        if local_res_binder.is_some() {
                            let SessionElementList(mut local_res_binder) =
                                local_res_binder.clone().unwrap();
                            st_cnt.append(&mut local_res_binder);
                        }
                        match session.get(&key) {
                            Some(SessionElementList(val)) => {
                                let val = val.clone(); // TODO Stupid clone
                                if val != st_cnt {
                                    println!(
                                        "\nEnvironment validation failed because Var {} is {} but should be {} according to binder.",
                                        key, SessionElementList(st_cnt), SessionElementList(val)
                                    );
                                    return false;
                                }
                                // if local_res_binder.is_some() {
                                //     println!("Local binder and var binder is contradictory. Unacceptable.");
                                //     return false;
                                // }
                            }
                            None => match local_res_binder {
                                Some(SessionElementList(local_res_binder)) => {
                                    if local_res_binder != st_cnt {
                                        println!("\nEnvironment validation failed because Local binder session type check for {} does not match: Expected {} but found {}. Validation failed.", key, SessionElementList(st_cnt), SessionElementList(local_res_binder));
                                        return false;
                                    }
                                }
                                None => {
                                    println!(
                                        "\nEnvironment validation failed because Var {} does not have a binder, cannot accept env.",
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
