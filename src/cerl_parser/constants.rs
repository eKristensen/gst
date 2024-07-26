use nom::{combinator::map, IResult};
use nom_supreme::error::ErrorTree;

use super::{ast::Const, expressions::literal};

pub fn constant(i: &str) -> IResult<&str, Const, ErrorTree<&str>> {
    map(literal, Const)(i)
}
