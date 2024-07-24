// Typing environment for GST type checker

use std::collections::HashMap;

use crate::contract_cerl::types::{BaseType, SessionTypesList};

#[derive(Debug, Clone)]
// Typing environments
pub enum TypeEnv {
    Gamma(SessionTypesList), // Constructor
    Delta(SessionTypesList), // Consume
    Sigma(BaseType),         // Base types
                             // Psi is statically included in contract core erlang
}

#[derive(Debug)]
// "Runtime" environment for type checker.
// TODO: Change type to be HasnMap<Var, TypeEnv> again !!
pub struct TypeEnvs(pub HashMap<String, TypeEnv>);
