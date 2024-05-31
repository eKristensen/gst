// Typing environment for GST type checker

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::Var,
    contract_cerl::types::{BaseType, SessionTypesList},
};

#[derive(Clone)]
// Typing environments
pub enum TypeEnv {
    Gamma(SessionTypesList), // Constructor
    Delta(SessionTypesList), // Consume
    Sigma(BaseType),         // Base types
                             // Psi is statically included in contract core erlang
}

// "Runtime" environment for type checker.
pub struct TypeEnvs(pub HashMap<Var, TypeEnv>);
