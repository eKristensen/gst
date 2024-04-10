mod analysis;
mod cerl_parser;
mod st_parser;

use cerl_parser::ast::Module;
use nom_supreme::error::ErrorTree;

use crate::analysis::{analyze_module::analyze_module_functions, init::init_funcs_env};

pub fn parse(src: &str) -> Result<(&str, cerl_parser::ast::Module), nom::Err<ErrorTree<&str>>> {
    cerl_parser::top::module(src)
}

pub fn analyze(m: Module) -> bool {
    let (env,_) = init_funcs_env(m);
    analyze_module_functions(&env)
}
