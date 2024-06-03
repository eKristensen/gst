use crate::{cerl_parser::ast::Var, contract_cerl::ast::CType};

use super::env::TypeEnvs;
use crate::type_checker::env::TypeEnv::Delta;
use crate::type_checker::env::TypeEnv::Gamma;
use crate::type_checker::env::TypeEnv::Sigma;

pub fn init_env(envs: &mut TypeEnvs, args: &Vec<Var>, spec: &Vec<CType>) -> Result<(), String> {
    // TODO: For all places .zip is used, length should be checked.
    // Rust makes no errors, just stops when size is mismatched, see
    // https://stackoverflow.com/questions/57345197/
    if args.len() != spec.len() {
        // Panic is acceptable as this should have been checked
        // before contract core erlang was constructed
        // Panic is better than silent acceptance
        return Err("args and spec do not have the same length".to_string());
    }
    for (var, elm_ctype) in args.iter().zip(spec.iter()) {
        let insert_res = match elm_ctype {
            CType::CBaseType(base_type) => envs
                .0
                .insert(var.clone(), Sigma(base_type.clone()))
                .is_none(),
            CType::CNewType(session_type) => envs
                .0
                .insert(var.clone(), Gamma(session_type.clone()))
                .is_none(),
            CType::CConsumeType(_, session_type) => envs
                .0
                .insert(var.clone(), Delta(session_type.clone()))
                .is_none(),
        };
        if !insert_res {
            return Err("Duplicate var in args, should not be possible!!!".to_string());
        }
    }
    Ok(())
}
