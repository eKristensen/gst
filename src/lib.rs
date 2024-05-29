mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;

use nom_supreme::error::ErrorTree;

pub fn parse(src: &str) -> Result<(&str, cerl_parser::ast::Module), nom::Err<ErrorTree<&str>>> {
    cerl_parser::top::module(src)
}

// pub fn analyze(m: Module) -> bool {
//     let (env,skipped) = init_funcs_env(m);
//     analyze_module_functions(&env, &skipped)
// }
