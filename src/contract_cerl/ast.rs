// Contract Core Erlang for type analysis

// Changes from cerl AST
// - Exprs no longer differentiate between <X> and X.
// - Exprs are converted to a tuple, as they act the same for analysis TODO: Might be a bad idea, we'll see later
// - Vec<Var> is converted to a pattern tuple, as with Exprs. TODO: Might be a bad idea, we'll see later.
// - Function calls must be atoms directly and not require evaluation to determine what function that are called
// - Add base and session spec type annotation to functions as part of their contract/signature
// - Remove "exports", we assume the erlang compiler will check function export
// - Remove "attributes". When specs are extracted they are not needed anymore.
// - Flatten functions (including handles) to pattern match on a top level
// - Some types of expressions are removed as they cannot be analyzed right now:
//   Fname, Map, Lambda functions, LetRec, Receive (gen server behavior must be used), Try, Catch

// TODO: Keep in mind: It might make sense to encode "is_xxx(Var)" (e.g. "when is_pid(X)") and other restrictions into the contract later on.
//       For now we rely completely on the specifications.

use std::collections::HashMap;

use crate::cerl_parser::ast::{Atom, FunName, Lit, Pat, Var};

use super::types::{BaseType, SessionTypesList};

// TODO: Support more than one module for each "program" definition. Could also make a generic-ish way to define build in functions by lookup instead of hardcoded.
#[derive(Debug)]
pub struct CModule {
    pub name: Atom,
    pub functions: HashMap<FunName, Vec<CFun>>,
}

// Problem above: There is a CExpr for each spec, many clauses. How to keep it?

#[derive(Debug, Clone)]
pub struct CFun {
    pub spec: Vec<CType>, // Spec acts as the functions "pattern", what type the arguments can be.
    pub args: Vec<Var>,   // Save argument names to make sense of function body.
    pub body: CExpr,
    pub return_type: BaseType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunCall {
    CPrimOp(Atom),     // Call basic erlang functions
    CApply(Atom),      // Inter-module call // Note: Apparently fname is used here
    CCall(Atom, Atom), // Cross-module call; (Module, Call name)
}

#[derive(Debug, Clone)]
//  Simplified expression for function body
pub enum CExpr {
    Var(Var),                         // E_base
    Lit(Lit),                         // E_base
    Cons(Vec<CExpr>),                 // E_base
    Tuple(Vec<CExpr>),                // E_base
    Let(Pat, Box<CExpr>, Box<CExpr>), // E_let*, TODO: Pat instead of var in let, OK?
    Case(Box<CExpr>, Vec<CClause>),   // E_case
    Call(CFunCall, Vec<CExpr>),       // E_{new,send, select, app}
    Do(Box<CExpr>, Box<CExpr>),
}

#[derive(Debug, Clone)]
pub struct CClause {
    pub pats: Vec<Pat>,
    // pub when: CExprs, // TODO: Note: "when" is not supported yet.
    pub res: CExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CType {
    CBaseType(BaseType),
    CNewType(SessionTypesList),
    CConsumeType(Option<Var>, SessionTypesList), // Option<Var> is used by the type_checker to identify sessions.
}
