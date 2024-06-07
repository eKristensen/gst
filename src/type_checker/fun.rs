use crate::{
    cerl_parser::ast::Atom,
    contract_cerl::{
        ast::{CExpr, CFunCall, CModule, CType},
        types::BaseType,
    },
};

use super::env::TypeEnvs;

// Handle build in functions
pub fn bif_fun(
    // module: &CModule,
    // envs: &mut TypeEnvs,
    call: &CFunCall,
    // args: &Vec<CExpr>,
) -> Result<CType, String> {
    // TODO: More clever way to handle BIF
    let bif_io_format = CFunCall::Call(Atom("io".to_owned()), Atom("format".to_owned()));
    if *call == bif_io_format {
        return Ok(CType::Base(BaseType::Atom(Atom("ok".to_string()))));
    }

    Err("Not bif".to_string())
}

// e_app
pub fn e_app(
    module: &CModule,
    envs: &mut TypeEnvs,
    call: &CFunCall,
    args: &Vec<CExpr>,
) -> Result<CType, String> {
    todo!()
}
