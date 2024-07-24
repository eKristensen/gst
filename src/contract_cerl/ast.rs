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
// - Supports more types: String, Char, Term, Float, Integer, but not Number.
// - Partial support for Maps and Lists specifications.

// TODO: Keep in mind: It might make sense to encode "is_xxx(Var)" (e.g. "when is_pid(X)") and other restrictions into the contract later on.
//       For now we rely completely on the specifications.

use core::fmt;
use std::collections::HashMap;

use crate::cerl_parser::ast::{Atom, LitInner, Var};

use super::types::{BaseType, SessionTypesList};

// TODO: Support more than one module for each "program" definition. Could also make a generic-ish way to define build in functions by lookup instead of hardcoded.
#[derive(Debug, Clone)]
pub struct CModule {
    pub name: Atom,
    pub functions: HashMap<CFunName, Vec<CFunClause>>,
}

// Problem above: There is a CExpr for each spec, many clauses. How to keep it?

#[derive(Debug, Clone)]
pub struct CFunClause {
    pub spec: Vec<CType>, // Spec acts as the functions "pattern", what type the arguments can be.
    pub args: Vec<Var>,   // Save argument names to make sense of function body.
    pub body: CExpr,
    pub return_type: BaseType,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CFunName {
    pub name: String,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunCall {
    PrimOp(Atom),         // Call basic erlang functions
    Apply(CFunName),      // Inter-module call // Note: Apparently fname is used here
    Call(String, String), // Cross-module call; (Module, Call name)
}
// TODO: Fix CFunCall::Call : Change argument types to Atom again.

#[derive(Debug, Clone)]
//  Simplified expression for function body
pub enum CExpr {
    Var(String),                       // E_base
    Lit(LitInner),                     // E_base
    Cons(Vec<CExpr>),                  // E_base
    Tuple(Vec<CExpr>),                 // E_base
    Let(CPat, Box<CExpr>, Box<CExpr>), // E_let*, TODO: Pat instead of var in let, OK?
    Case(Box<CExpr>, Vec<CClause>),    // E_case
    Call(CFunCall, Vec<CExpr>),        // E_{new,send, select, app}
    Do(Box<CExpr>, Box<CExpr>),
}

#[derive(Debug, Clone)]
pub struct CClause {
    pub pats: Vec<CPat>,
    // pub when: CExprs, // TODO: Note: "when" is not supported yet.
    pub res: CExpr,
}

#[derive(Debug, Clone)]
pub enum CPat {
    Var(Var),
    Lit(LitInner),
    Cons(Vec<CPat>),
    Tuple(Vec<CPat>),
    Alias(Var, Box<CPat>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CType {
    Base(BaseType),
    New(SessionTypesList),
    Consume(SessionTypesList),
}

#[derive(Debug)]
pub struct OptWarnings<T> {
    pub res: T,
    pub warnings: Vec<String>,
}

impl fmt::Display for CFunName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}
