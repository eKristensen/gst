// Represent spec type annotations
// Intermediate representation
// Goal: Make sense of Erlang tagged tuples and parse into a nice type to work with later

use std::collections::HashMap;

use crate::{cerl_parser::ast::FunNameInner, contract_cerl::types::BaseType};

pub struct BaseSpecDef(pub HashMap<FunNameInner, BaseSpecs>);

pub struct BaseSpecs(pub Vec<BaseSpec>);

pub struct BaseSpec {
    pub args: Vec<BaseSpecElm>,
    pub return_type: BaseType,
}

pub enum BaseSpecElm {
    Base(BaseType),
    New,
    Consume,
}
