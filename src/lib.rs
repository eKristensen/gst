mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;
mod type_checker;

use crate::contract_cerl::compose_contract::compose_contract;
use cerl_parser::{ast::Module, top::module};
use contract_cerl::ast::{CModule, OptWarnings};
use nom_supreme::{error::ErrorTree, final_parser::final_parser};

fn cerl_final(input: &str) -> Result<Module, ErrorTree<&str>> {
    final_parser(module)(input)
}

pub fn parse(src: &str) -> Result<OptWarnings<CModule>, ErrorTree<&str>> {
    let module = cerl_final(src);
    match module {
        Err(err) => {
            dbg!(err);
            panic!("See error debug above")
        }
        Ok(module) => Ok(compose_contract(module).unwrap()),
    }
}

pub fn type_check(m: CModule) -> OptWarnings<bool> {
    crate::type_checker::module::module(m)
}

// pub fn analyze(m: Module) -> bool {
//     let (env,skipped) = init functions env(m);
//     analyze_module_functions(&env, &skipped)
// }
