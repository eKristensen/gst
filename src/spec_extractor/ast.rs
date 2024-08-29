// Represent spec type annotations
// Intermediate representation
// Goal: Make sense of Erlang tagged tuples and parse into a nice type to work with later

use std::collections::HashMap;

use crate::{cerl_parser::ast::FunName, contract_cerl::types::BaseType};

pub struct BaseSpecDef(pub HashMap<FunName, BaseSpecs>);

pub struct BaseSpecs(pub Vec<BaseSpec>);

pub struct BaseSpec {
    pub args: Vec<BaseSpecElm>,
    pub return_type: BaseType,
}

#[derive(Debug)]
pub enum BaseSpecElm {
    Base(BaseType),
    New,
    Consume,
}
