// Typing environment for GST type checker

// Copy from other analysis files to here?

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{FunDef, FunName, Var},
    st_parser::ast::{SessionElementList, Types},
};

pub type Funcs = HashMap<FunName, FunEnv>; // Psi

#[derive(Debug, Clone)]
pub struct FunEnv {
    pub contract: Vec<FunContract>, // Merged -spec and -session
    pub return_type: FunContract,   // Return type for function
    pub body: Option<FunDef>,       // Body and var names for arguments
    pub must_analyze: bool,         // True if function lacks analysis
    pub comment: String,            // Info if must_analyze is false
}

// Argument types for function contracts and environment initialization
// Store -spec and -session info
#[derive(Debug, Clone)]
pub enum FunContract {
    Base(Types),
    New(SessionElementList),
    Ongoing(SessionElementList),
}

// Typing environments
pub struct TypeEnv {
    pub constructors: HashMap<Var, SessionElementList>, // Gamma
    pub ongoing: HashMap<Var, SessionElementList>,      // Delta
    pub base: HashMap<Var, Types>,                      // Sigma
}
