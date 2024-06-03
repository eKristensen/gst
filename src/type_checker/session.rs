use std::collections::{HashMap, HashSet};

use crate::{cerl_parser::ast::Var, contract_cerl::types::SessionType};

use super::env::{TypeEnv, TypeEnvs};

// Check that all sessions are finished
pub fn finished(envs: &TypeEnvs) -> Result<(), String> {
    let empty: TypeEnvs = TypeEnvs(HashMap::new());
    diff_consumed(&empty, envs)
}

pub fn diff_consumed(before_envs: &TypeEnvs, after_envs: &TypeEnvs) -> Result<(), String> {
    // println!("\ndiff consume check.\nBefore env: {:?}", before_envs);
    // println!("After envs: {:?}", after_envs);

    // Check all that should be consumed, has been consumed (aka "finished" function)
    // Two stage: Find all to check, and then to check their value.
    let before_vars = before_envs.0.keys().cloned().collect();
    let after_vars: HashSet<Var> = after_envs.0.keys().cloned().collect();
    let must_be_consumed: HashSet<&Var> = after_vars.difference(&before_vars).collect();
    for check_var in must_be_consumed {
        match after_envs.0.get(check_var).unwrap() {
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
                    return Err(format!("{:?} Not consumed {:?}", check_var, check_consumed));
                }
                return Err(format!("{:?} Not consumed {:?}", check_var, check_consumed));
            }
            TypeEnv::Sigma(_) => continue,
        }
    }
    Ok(())
}
