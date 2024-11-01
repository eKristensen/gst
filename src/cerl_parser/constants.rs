use nom::{combinator::map, IResult};
use nom_supreme::error::ErrorTree;

use super::{ast::Const, cinput::CInput, expressions::literal};

// WSA OK
pub fn constant(i: CInput) -> IResult<CInput, Const, ErrorTree<CInput>> {
    map(literal, Const)(i)
}
