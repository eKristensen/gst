mod analysis;
mod cerl_parser;
mod st_parser;

use cerl_parser::ast::Module;

use crate::analysis::{analyze_var::analyze_module, compute_init_env::init_module_env};

pub fn parse(
    src: &str,
) -> Result<(&str, cerl_parser::ast::Module), nom::Err<nom::error::Error<&str>>> {
    cerl_parser::top::module(src)
}

pub fn analyze(m: Module) -> bool {
    let env = init_module_env(m);
    analyze_module(&env)
}
