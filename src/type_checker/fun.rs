use crate::contract_cerl::ast::{CExpr, CFunCall, CModule, CType};

use super::env::TypeEnvs;

// Handle build in functions
pub fn bif_fun(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    todo!()
}
