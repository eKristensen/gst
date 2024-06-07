use std::collections::HashMap;

use crate::contract_cerl::ast::{CModule, CType, OptWarnings};

use super::{base::expr, env::TypeEnvs, init::init_env, session::finished};

// Entry point for type checking.
// One module in, no environment required
// Return acceptance or failure and a list of messages
pub fn module(module: CModule) -> OptWarnings<bool> {
    if module.functions.is_empty() {
        return OptWarnings {
            res: false,
            warnings: vec![format!("Nothing to analyze, not acceptable")],
        };
    }
    let mut warnings: Vec<String> = Vec::new();
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
                warnings.push(format!(
                    "Init env failed for {} due to {}",
                    fun_name,
                    init_ok.err().unwrap()
                ));
                overall_acceptance = false;
                continue;
            }

            // Type check body, updates envs in place
            // module is sent along to give access to function signatures
            let return_type = expr(&module, &mut envs, &clause.body);
            if return_type.is_err() {
                warnings.push(format!(
                    "Type checking failed for {} due to {}",
                    fun_name,
                    return_type.err().unwrap()
                ));
                overall_acceptance = false;
                continue;
            }

            // Check return-type somehow?
            // TODO: Possible performance optimization:
            // Instead of clone, unpack return_type and compare directly
            let clause_actual_return_type = return_type.unwrap();
            if clause_actual_return_type != CType::Base(clause.return_type.clone()) {
                warnings.push(format!(
                    "Wrong return type for function {}. Expected {:?} but found {:?}",
                    fun_name, clause.return_type, clause_actual_return_type
                ));
                overall_acceptance = false;
            }

            // Check result, i.e. everything that should be consumed, has been consumed
            if let Err(err_val) = finished(&envs) {
                warnings.push(format!("Function contract for {} does not hold as all sessions are not fully consumed: {}", fun_name, err_val));
                overall_acceptance = false
            };

            // If something went wrong set overall_acceptance to false
        }
    }
    OptWarnings {
        res: overall_acceptance,
        warnings,
    }
}
