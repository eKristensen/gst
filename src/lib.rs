mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;
mod type_checker;

use crate::contract_cerl::compose_contract::compose_contract;
use crate::type_checker::module::module;
use contract_cerl::ast::CModule;
use nom_supreme::error::ErrorTree;

pub fn parse(src: &str) -> Result<CModule, nom::Err<ErrorTree<&str>>> {
    let (_, module) = cerl_parser::top::module(src)?;
    Ok(compose_contract(module).unwrap())
}

pub fn type_check(m: CModule) -> () {
    module(m);
    todo!()
}

// pub fn analyze(m: Module) -> bool {
//     let (env,skipped) = init functions env(m);
//     analyze_module_functions(&env, &skipped)
// }
