use std::collections::HashMap;

use crate::contract_cerl::ast::CModule;

use super::{env::TypeEnvs, expr::expr, init::init_env, session::finished};
use crate::contract_cerl::ast::CType::CBaseType;

// Entry point for type checking.
// One module in, no environment required
// Return acceptance or failure and a list of messages
pub fn module(module: CModule) -> (bool, Vec<String>) {
    if module.functions.len() == 0 {
        return (false, vec![format!("Nothing to analyze, not acceptable")]);
    }
    // Overall acceptance. Assume all is OK until proven otherwise
    let mut overall_acceptance = true;
    // each function get a fresh env
    for (fun_name, clauses) in &module.functions {
        // Check each function clause
        for clause in clauses {
            // Define envs hashmap for this clause
            let mut envs: TypeEnvs = TypeEnvs(HashMap::new());

            // Init env based on function header
            let init_ok = init_env(&mut envs, &clause.args, &clause.spec);
            if init_ok.is_err() {
                println!(
                    "Init env failed for {} due to {}",
                    fun_name,
                    init_ok.err().unwrap()
                );
                overall_acceptance = false;
                continue;
            }

            // Type check body, updates envs in place
            // module is sent along to give access to function signatures
            let return_type = expr(&module, &mut envs, &clause.body);
            if return_type.is_err() {
                println!(
                    "Type checking failed for {} due to {}",
                    fun_name,
                    return_type.err().unwrap()
                );
                overall_acceptance = false;
                continue;
            }

            // Check return-type somehow?
            // TODO: Possible performance optimization:
            // Instead of clone, unpack return_type and compare directly
            if return_type.unwrap() != CBaseType(clause.return_type.clone()) {
                // Add msg that return type is bad
                overall_acceptance = false;
            }

            // Check result, i.e. everything that should be consumed, has been consumed
            if finished(&envs).is_err() {
                println!("not finished");
                overall_acceptance = false
            };

            // If something went wrong set overall_acceptance to false
        }
    }
    println!("overall acceptance {}", overall_acceptance);
    todo!("module finish impl")
}
