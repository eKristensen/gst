mod cerl_parser;

pub fn parse(
    src: &str,
) -> Result<(&str, cerl_parser::ast::Module), nom::Err<nom::error::Error<&str>>> {
    cerl_parser::top::module(src)
}
