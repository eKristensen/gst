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

use std::{collections::HashMap, rc::Rc};

use crate::cerl_parser::ast::{Atom, CLoc, FunName, Lit, Var};

use super::types::{BaseType, SessionTypesList};

// TODO: Support more than one module for each "program" definition. Could also make a generic-ish way to define build in functions by lookup instead of hardcoded.
#[derive(Debug, Clone)]
pub struct CModule {
    pub name: Rc<Atom>,
    pub mspec: Option<SessionTypesList>, // If module implements gen_server_plus behavior save mspec here
    pub functions: HashMap<Rc<FunName>, Vec<CFunClause>>,
    pub fallback_args: HashMap<Rc<FunName>, Vec<Var>>, // If clause has literal, we need to bind it to
                                                       // the top level var name.
}

// Problem above: There is a CExpr for each spec, many clauses. How to keep it?

#[derive(Debug, Clone)]
pub struct CFunClause {
    pub spec: Vec<CType>, // Spec acts as the functions "pattern", what type the arguments can be.
    pub args: Vec<CPat>,  // Save argument names to make sense of function body.
    pub body: CExpr,
    pub return_type: BaseType,
}

#[derive(Debug, Clone)]
//  Simplified expression for function body
pub enum CExpr {
    Var(Rc<CLoc>, Rc<Var>),                        // E_base
    Lit(Rc<CLoc>, Rc<Lit>),                        // E_base
    Cons(Rc<CLoc>, Vec<CExpr>),                    // E_base
    Tuple(Rc<CLoc>, Vec<CExpr>),                   // E_base
    Let(Rc<CLoc>, Rc<CPat>, Rc<CExpr>, Rc<CExpr>), // E_let*
    Case(Rc<CLoc>, Rc<CExpr>, Vec<CClause>),       // E_case
    PrimOp(Rc<CLoc>, Rc<Atom>, Vec<CExpr>),
    Apply(Rc<CLoc>, Rc<FunName>, Vec<CExpr>),
    Call(Rc<CLoc>, Rc<Atom>, Rc<Atom>, Vec<CExpr>), // E_{new,send, select, app}
    Do(Rc<CLoc>, Rc<CExpr>, Rc<CExpr>),
}

#[derive(Debug, Clone)]
pub struct CClause {
    pub pats: Vec<CPat>,
    // pub when: CExprs, // TODO: Note: "when" is not supported yet.
    pub res: CExpr,
}

#[derive(Debug, Clone)]
pub enum CPat {
    Any, // Emulate _ when variable is not needed, e.g. for do e1 e2
    Var(Rc<CLoc>, Rc<Var>),
    Lit(Rc<CLoc>, Rc<Lit>),
    Cons(Rc<CLoc>, Vec<CPat>),
    Tuple(Rc<CLoc>, Vec<CPat>),
    Alias(Rc<CLoc>, Rc<Var>, Rc<CPat>),
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
