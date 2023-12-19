
mod parser;

pub fn parse(src: &str) -> Result<(&str, parser::ast::Module), nom::Err<nom::error::Error<&str>>> {
    parser::top::module(src)
}