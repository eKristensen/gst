use nom::{combinator::map, IResult};
use nom_supreme::error::ErrorTree;

use super::{ast::Const, expressions::literal, helpers::CInput};

// WSA OK
pub fn constant(i: CInput) -> IResult<CInput, Const, ErrorTree<&str>> {
    map(literal, Const)(i)
}
