// Typing environment for GST type checker

// Copy from other analysis files to here?

use core::fmt;
use std::collections::HashMap;

use crate::{
    cerl_parser::ast::{FunDef, FunName, Var},
    st_parser::ast::{SessionDef, SessionElementList, SessionType, Types},
};

// TODO: I have a feeling that I should be able to refactor this type away
#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Base(Types),
    ST(SessionType),
}

#[derive(Debug, Clone)]
pub struct FunEnv {
    pub spec: Option<(Vec<Types>, Types)>, // TODO: Too simple to be useful in the long run (no alternative types on top-level)
    pub session: Option<SessionDef>,
    pub body: Option<FunDef>,
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarType::Base(res) => write!(f, "{}", res),
            VarType::ST(res) => write!(f, "{}", res),
        }
    }
}

// Common
pub struct TypeEnv {
    pub constructors: HashMap<Var, SessionElementList>, // Gamma
    pub ongoing: HashMap<Var, SessionElementList>,      // Delta
    pub base: HashMap<Var, VarType>,                    // Sigma
    pub funcs: HashMap<FunName, FunEnv>,                // Psi
}
