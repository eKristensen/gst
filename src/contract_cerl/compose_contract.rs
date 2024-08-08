// Translator from Core Erlang AST to Contract Core Erlang

use std::collections::HashMap;

use crate::{
    cerl_parser::{
        self,
        ast::{Anno, AnnoExpr, Atom, Clause, Const, Expr, FunCall, FunDef, FunName, Lit, Pat},
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
    ast::{self, CClause, CExpr, CFunClause, CModule, CPat, CType, OptWarnings},
    types::BaseType,
};
use crate::contract_cerl::ast::CFunCall;
use crate::spec_extractor::ast::BaseSpecElm::Base;
use crate::st_parser::ast::SessionSpecElm::ConsumeSpec;
use crate::st_parser::ast::SessionSpecElm::NewSpec;

pub fn compose_contract(
    ast: cerl_parser::ast::AnnoModule,
) -> Result<OptWarnings<ast::CModule>, String> {
    // Step 1: Resolve dependencies: Must get specs
    let base_spec = base_spec_extractor(&ast.inner)?;
    let session_spec = session_spec_extractor(&ast.inner)?;

    // Step 2: Convert while matching specs.
    Ok(make_contract(&ast.inner, base_spec, session_spec))
}

fn make_contract(
    ast: &cerl_parser::ast::Module,
    base_spec: BaseSpecDef,
    session_spec: SessionSpecDef,
) -> OptWarnings<ast::CModule> {
    let mut warnings: Vec<String> = Vec::new();
    let mut functions: HashMap<FunName, Vec<CFunClause>> = HashMap::new();
    // One body for each module
    for fun_def in &ast.defs {
        let fname = fun_def.name.inner.clone();
        match compose_function_with_contract(&base_spec, &session_spec, fun_def) {
            Ok(contract_clauses) => {
                functions.insert(fname, contract_clauses);
            }
            Err(err) => {
                // TODO: Consider to make a strict mode that requires full annotation
                if fname.name.0 == "module_info" && (fname.arity == 0 || fname.arity == 1) {
                    // Ignore 'module_info'/0 and 'module_info'/1
                    continue;
                }
                warnings.push(format!(
                    "Function {} could not be included in Contract Core Erlang because {}",
                    fname, err
                ));
            }
        };
    }
    OptWarnings {
        res: CModule {
            name: ast.name.clone(),
            functions,
        },
        warnings,
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
                clause_spec.push(CType::New(session_elm.clone()))
            }
            (BaseSpecElm::Consume, ConsumeSpec(session_elm)) => {
                clause_spec.push(CType::Consume(session_elm.clone()))
            }
            (Base(base_elm), SessionSpecElm::BasePlaceholder) => {
                clause_spec.push(CType::Base(base_elm.clone()))
            }
            _ => return Err("Bad spec.".to_string()),
        }
    }
    Ok(clause_spec)
}

fn compose_function_with_contract(
    base_spec: &BaseSpecDef,
    session_spec: &SessionSpecDef,
    fun_def: &FunDef,
) -> Result<Vec<CFunClause>, String> {
    // TODO: Maybe each function should be a function on their own with a Result type so we can fail for a single function and process the next one
    //       Yes, that makes way too much sense. Gotta do that ...
    let fname = &fun_def.name.inner;

    // fname is the fun-name.
    // WF check makes a lot of sense here....
    if !base_spec.0.contains_key(fname) {
        return Err(format!(
            "Function {} is excluded from contract erlang as it got no base spec",
            fname
        ));
    }
    let this_base_spec = base_spec.0.get(fname).unwrap();

    if !session_spec.0.contains_key(fname) {
        return Err(format!(
            "Function {} is excluded from contract erlang as it got no session spec",
            fname
        ));
    }
    let this_session_spec = session_spec.0.get(fname).unwrap();

    if this_base_spec.0.len() != this_session_spec.0.len() {
        return Err(format!(
            "Function {} is excluded due to mismatch in spec length",
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

    // The first expression in any function definition is a function.
    let fun_expr = &fun_def.body.fun;

    // A top-level function expression may contain multiplte clauses.
    // If there are many clauses they will be contained within a normal case expression as the
    // first expression within the function expression body.
    // We check for this annotation. If the annotation is not present the function only has one
    // clause.
    let clauses = match &fun_expr.body.inner {
        Expr::Case(_top_cases_var, top_cases_clauses) => {
            match &fun_expr.body.anno.0 {
                None => {
                    let cexpr = expr_to_cexpr(&fun_expr.body.inner)?;
                    vec![cexpr]
                }
                Some(anno) => {
                    todo!("Found top-level case. This can be because there are multiplte clauess or a top-levle case. Check annotation: {:?}", fun_expr.body.anno);
                    let mut clauses_res: Vec<CExpr> = Vec::new();

                    // TODO: We throw away "when" right now. Include agian later. It will be needed for
                    // proper type checking later.
                    for clause in top_cases_clauses {
                        let cexpr = expr_to_cexpr(&clause.inner.res.inner)?;
                        clauses_res.push(cexpr);
                    }
                    clauses_res
                }
            }
        }

        single_clause => match expr_to_cexpr(single_clause) {
            Ok(cexpr) => vec![cexpr],
            _ => todo!("Unable to to process function expression"),
        },
    };

    if partial_cfun.len() != clauses.len() {
        return Err(format!(
            " Function {} is excluded due to mismatch in number of spec and body clauses",
            fname
        ));
    }

    // Match ctype spec and clauses
    let mut contract_clauses: Vec<CFunClause> = Vec::new();
    for (partial_ctype_elm, this_body) in partial_cfun.iter().zip(clauses.iter()) {
        // It should be a very simple, "just add" stuff here
        contract_clauses.push(
            // TODO: Deduplicate args, but moving to function name does not seem like a good solution.
            CFunClause {
                spec: partial_ctype_elm.spec.clone(),
                args: fun_def
                    .clone()
                    .body
                    .fun
                    .vars
                    .into_iter()
                    .map(|v| v.name)
                    .collect(),
                body: this_body.clone(),
                return_type: partial_ctype_elm.return_type.clone(),
            },
        )
    }
    Ok(contract_clauses)
}

// TODO: Can clause_to_cclause and expr_to_cexpr be "mered" into one name via some trait or
// similar?

fn clause_to_cclause(clause: &Clause) -> Result<CClause, String> {
    let res = expr_to_cexpr(&clause.res.inner)?;
    Ok(CClause {
        pats: clause
            .clone()
            .pats
            .into_iter()
            .map(|p| pat_to_cpat(p.inner))
            .collect(),
        res,
    })
}

fn pat_to_cpat(pat: Pat) -> CPat {
    match pat {
        Pat::Var(v) => CPat::Var(v.name),
        Pat::Lit(l) => CPat::Lit(l),
        Pat::Cons(pat_cons) => {
            CPat::Cons(pat_cons.into_iter().map(|p| pat_to_cpat(p.inner)).collect())
        }
        Pat::Tuple(pat_tuple) => CPat::Tuple(
            pat_tuple
                .into_iter()
                .map(|p| pat_to_cpat(p.inner))
                .collect(),
        ),
        Pat::Alias(var, pat) => CPat::Alias(var.name, Box::new(pat_to_cpat(pat.inner))),
    }
}

fn expr_to_cexpr(expr: &Expr) -> Result<CExpr, String> {
    match expr {
        Expr::Var(v) => Ok(CExpr::Var(v.clone())),
        Expr::AtomLit(l) => Ok(CExpr::Lit(l.clone())),
        Expr::Cons(exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = expr_to_cexpr(&expr.inner)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Cons(tuple))
        }
        Expr::Exprs(exprs) => {
            // Convert to direct value if exprs is only one long
            if exprs.len() == 1 {
                let [exprs] = exprs.as_slice() else {
                    return Err("Expected only on expr".to_string());
                };
                expr_to_cexpr(&exprs.inner)
            } else {
                let mut tuple: Vec<CExpr> = Vec::new();
                for expr in exprs {
                    let cexpr = expr_to_cexpr(&expr.inner)?;
                    tuple.push(cexpr)
                }
                Ok(CExpr::Tuple(tuple))
            }
        }
        Expr::Tuple(exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = expr_to_cexpr(&expr.inner)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Tuple(tuple))
        }
        Expr::Let(v, e1, e2) => {
            let v = match v.as_slice() {
                [v] => CPat::Var(v.name.clone()),
                _ => CPat::Tuple(v.iter().map(|v| CPat::Var(v.name.clone())).collect()),
            };
            let e1 = expr_to_cexpr(&e1.inner)?;
            let e2 = expr_to_cexpr(&e2.inner)?;

            Ok(CExpr::Let(v, Box::new(e1), Box::new(e2)))
        }
        Expr::Case(e1, e2) => {
            let base_expr = expr_to_cexpr(&e1.inner)?;
            let mut contract_clauses: Vec<CClause> = Vec::new();
            for clause in e2 {
                if Anno(Some(vec![Const(Lit::Atom(Atom(
                    "compiler_generated".to_string(),
                )))]))
                    == clause.anno
                {
                    println!("Warning: Skipped compiler generated case."); // TODO: Proper warning
                    continue;
                }
                let res = clause_to_cclause(&clause.inner)?;
                contract_clauses.push(res);
            }
            Ok(CExpr::Case(Box::new(base_expr), contract_clauses))
        }
        Expr::Call(fun_call, args) => {
            let fun_call = match fun_call.as_ref().clone() {
                FunCall::PrimOp(AnnoExpr {
                    anno: _anno,
                    inner: Expr::AtomLit(Lit::Atom(a)),
                }) => CFunCall::PrimOp(a),
                FunCall::Apply(AnnoExpr {
                    anno: _anno,
                    inner: Expr::Fname(f),
                }) => CFunCall::Apply(f),
                FunCall::Call(
                    AnnoExpr {
                        anno: _anno0,
                        inner: Expr::AtomLit(Lit::Atom(module_name)),
                    },
                    AnnoExpr {
                        anno: _anno1,
                        inner: Expr::AtomLit(Lit::Atom(call_name)),
                    },
                ) => CFunCall::Call(module_name, call_name),
                _ => return Err("Function call arguments has unsupported types.".to_string()),
            };

            let mut args_cexpr: Vec<CExpr> = Vec::new();
            for arg in args {
                let cexpr = expr_to_cexpr(&arg.inner)?;
                args_cexpr.push(cexpr);
            }

            Ok(CExpr::Call(fun_call, args_cexpr))
        }
        Expr::Do(e1, e2) => {
            let e1 = expr_to_cexpr(&e1.inner)?;
            let e2 = expr_to_cexpr(&e2.inner)?;
            Ok(CExpr::Do(Box::new(e1), Box::new(e2)))
        }
        _ => Err("Expression not supported in Contract Core Erlang yet.".to_string()),
    }
}
