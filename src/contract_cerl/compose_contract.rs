// Translator from Core Erlang AST to Contract Core Erlang

use core::panic;
use std::collections::HashMap;

use crate::{
    cerl_parser::{
        self,
        ast::{FunCall, FunDef, FunName, Lit, Pat},
    },
    spec_extractor::{
        ast::{BaseSpec, BaseSpecDef, BaseSpecElm},
        extractor::base_spec_extractor,
    },
    st_parser::{
        ast::{SessionSpec, SessionSpecDef, SessionSpecElm},
        spec_extractor::session_spec_extractor,
    },
};

use super::{
    ast::{self, CClause, CExpr, CFun, CModule, CType},
    types::BaseType,
};
use crate::contract_cerl::ast::CFunCall;
use crate::contract_cerl::compose_contract::CType::CBaseType;
use crate::contract_cerl::compose_contract::CType::CConsumeType;
use crate::contract_cerl::compose_contract::CType::CNewType;
use crate::spec_extractor::ast::BaseSpecElm::Base;
use crate::st_parser::ast::SessionSpecElm::ConsumeSpec;
use crate::st_parser::ast::SessionSpecElm::NewSpec;

pub fn compose_contract(ast: cerl_parser::ast::Module) -> ast::CModule {
    // Step 1: Resolve dependencies: Must get specs
    let base_spec = base_spec_extractor(&ast);
    let session_spec = session_spec_extractor(&ast);

    // Step 2: Convert while matching specs.
    make_contract(&ast, base_spec, session_spec)
}

fn make_contract(
    ast: &cerl_parser::ast::Module,
    base_spec: BaseSpecDef,
    session_spec: SessionSpecDef,
) -> ast::CModule {
    let mut functions: HashMap<FunName, Vec<CFun>> = HashMap::new();
    // One body for each module
    for (fname, def) in ast.body.clone() {
        match compose_function_with_contract(&base_spec, &session_spec, &fname, def) {
            Ok(contract_clauses) => {
                functions.insert(fname, contract_clauses);
            }
            Err(err) => {
                println!(
                    "Warning: Function {} could not be included in Contract Core Erlang because {}",
                    fname, err
                );
            }
        };
    }
    CModule {
        name: ast.name.clone(),
        functions,
    }
}

fn merge_base_session_spec(
    clause_base_elm: &BaseSpec,
    clause_session_elm: &SessionSpec,
) -> Result<Vec<CType>, String> {
    let mut clause_spec: Vec<CType> = Vec::new();
    for spec_elms in clause_base_elm.args.iter().zip(clause_session_elm.0.iter()) {
        // zip/merge specs
        match spec_elms {
            (BaseSpecElm::New, NewSpec(session_elm)) => {
                clause_spec.push(CNewType(session_elm.clone()))
            }
            (BaseSpecElm::Consume, ConsumeSpec(session_elm)) => {
                clause_spec.push(CConsumeType(session_elm.clone()))
            }
            (Base(base_elm), SessionSpecElm::BasePlaceholder) => {
                clause_spec.push(CBaseType(base_elm.clone()))
            }
            _ => return Err(format!("Warning: Bad spec.")),
        }
    }
    Ok(clause_spec)
}

fn compose_function_with_contract(
    base_spec: &BaseSpecDef,
    session_spec: &SessionSpecDef,
    fname: &FunName,
    def: FunDef,
) -> Result<Vec<CFun>, String> {
    // TODO: Maybe each function should be a function on their own with a Result type so we can fail for a single function and process the next one
    //       Yes, that makes way too much sense. Gotta do that ...

    // fname is the fun-name.
    // WF check makes a lot of sense here....
    if !base_spec.0.contains_key(&fname) {
        // TODO: A better way to handle warnings?
        return Err(format!(
            "Warning: Function {} is excluded from contract erlang as it got no base spec",
            fname
        ));
    }
    let this_base_spec = base_spec.0.get(&fname).unwrap();

    if !session_spec.0.contains_key(&fname) {
        // TODO: A better way to handle warnings?
        return Err(format!(
            "Warning: Function {} is excluded from contract erlang as it got no session spec",
            fname
        ));
    }
    let this_session_spec = session_spec.0.get(&fname).unwrap();

    if this_base_spec.0.len() != this_session_spec.0.len() {
        // TODO: A better way to handle warnings?
        return Err(format!(
            "Warning: Function {} is excluded due to mismatch in spec length",
            fname
        ));
    }

    // Type for CFun without body and args
    struct PartialCFun {
        spec: Vec<CType>,
        return_type: BaseType,
    }
    // Match specs first
    let mut partial_cfun: Vec<PartialCFun> = Vec::new();
    for (clause_base_elm, clause_session_elm) in
        this_base_spec.0.iter().zip(this_session_spec.0.iter())
    {
        // Each clause spec content
        let clause_spec = merge_base_session_spec(clause_base_elm, clause_session_elm)?;
        partial_cfun.push(PartialCFun {
            spec: clause_spec,
            return_type: clause_base_elm.return_type.clone(),
        })
    }

    // We assume top-level Exprs follow the format of a top-level function specification at this point
    // Top level function can both be a case or a direct expression. Direct means there is only one clause.
    let cerl_parser::ast::Exprs::One(fun_body) = def.body else {
        panic!("Top-level fun def body are not expected to be a value list")
    };

    // Get the list of clauses
    let clauses = match *fun_body {
        cerl_parser::ast::Expr::Case(_args_again, clauses) => {
            // TODO: "when" (clause.when) is ignored here!!!!
            // TODO fold/map-like instead? How to make "?" work inside map ?
            let mut clauses_res: Vec<CExpr> = Vec::new();
            for clause in clauses {
                let cexpr = exprs_to_cexpr(clause.res)?;
                clauses_res.push(cexpr);
            }
            clauses_res
        }
        single_clause => match expr_to_cexpr(single_clause) {
            Ok(cexpr) => vec![cexpr],
            _ => vec![],
        },
    };

    if partial_cfun.len() != clauses.len() {
        // TODO: A better way to handle warnings?
        return Err(format!(
            "Warning: Function {} is excluded due to mismatch in number of spec and body clauses",
            fname
        ));
    }

    // Match ctype spec and clauses
    let mut contract_clauses: Vec<CFun> = Vec::new();
    for (partial_ctype_elm, this_body) in partial_cfun.iter().zip(clauses.iter()) {
        // It should be a very simple, "just add" stuff here
        contract_clauses.push(
            // TODO: Deduplicate args, but moving to funname does not seem like a good solution.
            CFun {
                spec: partial_ctype_elm.spec.clone(),
                args: def.args.clone(),
                body: this_body.clone(),
                return_type: partial_ctype_elm.return_type.clone(),
            },
        )
    }
    Ok(contract_clauses)
}

fn expr_to_cexpr(expr: cerl_parser::ast::Expr) -> Result<CExpr, String> {
    match expr {
        cerl_parser::ast::Expr::Var(v) => Ok(CExpr::Var(v)),
        cerl_parser::ast::Expr::Lit(l) => Ok(CExpr::Lit(l)),
        cerl_parser::ast::Expr::Cons(exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = exprs_to_cexpr(expr)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Cons(tuple))
        }
        cerl_parser::ast::Expr::Tuple(exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = exprs_to_cexpr(expr)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Tuple(tuple))
        }
        cerl_parser::ast::Expr::Let(v, e1, e2) => {
            let e1 = exprs_to_cexpr(e1)?;
            let e2 = exprs_to_cexpr(e2)?;
            // TODO: Note: Multi-value let convert to tuple again.
            let clause = CClause {
                pats: vec![Pat::Tuple(v.iter().map(|x| Pat::Var(x.clone())).collect())],
                res: e2,
            };
            Ok(CExpr::Case(Box::new(e1), vec![clause]))
        }
        cerl_parser::ast::Expr::Case(e1, e2) => {
            let base_expr = exprs_to_cexpr(e1)?;
            let mut contract_clauses: Vec<CClause> = Vec::new();
            for clause in e2 {
                let res = exprs_to_cexpr(clause.res)?;
                contract_clauses.push(CClause {
                    pats: clause.pats,
                    res,
                });
            }
            Ok(CExpr::Case(Box::new(base_expr), contract_clauses))
        }
        cerl_parser::ast::Expr::Call(fun_call, args) => {
            let fun_call = match fun_call {
                FunCall::PrimOp(a) => CFunCall::CPrimOp(a),
                FunCall::Apply(f) => CFunCall::CApply(f.name),
                FunCall::Call(module_name, call_name) => {
                    let module_name = exprs_to_cexpr(module_name)?;
                    let CExpr::Lit(Lit::Atom(module_name)) = module_name else {
                        return Err(format!("Function call name must be an atom"));
                    };
                    let call_name = exprs_to_cexpr(call_name)?;
                    let CExpr::Lit(Lit::Atom(call_name)) = call_name else {
                        return Err(format!("Function call name must be an atom"));
                    };
                    CFunCall::CCall(module_name, call_name)
                }
            };

            let mut args_cexpr: Vec<CExpr> = Vec::new();
            for arg in args {
                let cexpr = exprs_to_cexpr(arg)?;
                args_cexpr.push(cexpr);
            }

            Ok(CExpr::Call(fun_call, args_cexpr))
        }
        cerl_parser::ast::Expr::Do(e1, e2) => {
            let e1 = exprs_to_cexpr(e1)?;
            let e2 = exprs_to_cexpr(e2)?;
            // TODO: Does empty pattern work for do or will it just never get matched?
            Ok(CExpr::Case(
                Box::new(e1),
                vec![CClause {
                    pats: vec![],
                    res: e2,
                }],
            ))
        }
        _ => Err(format!(
            "Expression not supported in Contract Core Erlang used."
        )),
    }
}

fn exprs_to_cexpr(exprs: cerl_parser::ast::Exprs) -> Result<CExpr, String> {
    match exprs {
        cerl_parser::ast::Exprs::One(expr) => expr_to_cexpr(*expr),
        cerl_parser::ast::Exprs::Many(exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = expr_to_cexpr(expr)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Tuple(tuple))
        }
    }
}
