use std::collections::HashMap;

use crate::contract_cerl::ast::CModule;

use super::{env::TypeEnvs, expr::expr, init::init_env, session::finished};
use crate::contract_cerl::ast::CType::CBaseType;

// Entry point for type checking.
// One module in, no environment required
// Return acceptance or failure and a list of messages
pub fn module(module: CModule) -> (bool, Vec<String>) {
    // Overall acceptance. Assume all is OK until proven otherwise
    let mut overall_acceptance = true;
    // each function get a fresh env
    for (fun_name, clauses) in &module.functions {
        // Check each function clause
        for clause in clauses {
            // Define envs hashmap for this clause
            let mut envs: TypeEnvs = TypeEnvs(HashMap::new());

            // Init env based on function header
            init_env(&mut envs, &clause.args, &clause.spec);

            // Type check body, updates envs in place
            // module is sent along to give access to function signatures
            let return_type = expr(&module, &envs, &clause.body);

            // Check return-type somehow?
            // TODO: Possible performance optimization:
            // Instead of clone, unpack return_type and compare directly
            if return_type != CBaseType(clause.return_type.clone()) {
                // Add msg that return type is bad
                overall_acceptance = false;
            }

            // Check result, i.e. everything that should be consumed, has been consumed
            finished(&envs);

            // If something went wrong set overall_acceptance to false
        }
    }
    todo!("module finish impl")
}
