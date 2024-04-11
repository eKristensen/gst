// Typing environment for GST type checker

// TODO: Duplicate variable definition is hard to detect when the environment are in disjoint maps.
// Add some sanity check on top? A guard? A HashSet with all defined variables?
// Maybe a "class" so to say? Where we can get and set from the environments with guards?

use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{FunDef, FunName, Var},
    st_parser::ast::{SessionElementList, Types},
};

pub type Funcs = HashMap<FunName, FunEnv>; // Psi

#[derive(Debug, Clone)]
pub struct FunEnv {
    pub contract: Vec<FunContract>, // Merged -spec and -session
    pub return_type: Types,   // Return type for function
    pub body: Option<FunDef>,       // Body and var names for arguments
}

// Argument types for function contracts and environment initialization
// Store -spec and -session info
#[derive(Debug, Clone, PartialEq)]
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
