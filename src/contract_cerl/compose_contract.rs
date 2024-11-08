// Translator from Core Erlang AST to Contract Core Erlang

use std::{borrow::Borrow, collections::HashMap, rc::Rc};

use crate::{
    cerl_parser::{
        self,
        ast::{
            Anno, AnnoPat, Atom, CallModule, Clause, Const, Expr, FunDef, FunName, Lit, Pat, Var,
        },
    },
    spec_extractor::{
        ast::{BaseSpec, BaseSpecDef, BaseSpecElm},
        extractor::base_spec_extractor,
    },
    st_parser::{
        ast::{SessionSpec, SessionSpecDef, SessionSpecElm},
        spec_extractor::{mspec_extractor, session_spec_extractor},
    },
    type_checker::env::CastEnv,
};

use super::{
    ast::{self, CClause, CExpr, CFunClause, CModule, CPat, CType, OptWarnings},
    types::{BaseType, SessionTypesList},
};
use crate::spec_extractor::ast::BaseSpecElm::Base;
use crate::st_parser::ast::SessionSpecElm::ConsumeSpec;
use crate::st_parser::ast::SessionSpecElm::NewSpec;

pub fn compose_contract(
    ast: cerl_parser::ast::AnnoModule,
) -> Result<OptWarnings<ast::CModule>, String> {
    // Step 1: Resolve dependencies: Must get specs
    let base_spec = base_spec_extractor(&ast.inner)?;
    let session_spec = session_spec_extractor(&ast.inner)?;
    let mspec = mspec_extractor(&ast.inner)?;

    // Step 2: Convert while matching specs.
    Ok(make_contract(&ast.inner, base_spec, session_spec, mspec))
}

fn make_contract(
    ast: &cerl_parser::ast::Module,
    base_spec: BaseSpecDef,
    session_spec: SessionSpecDef,
    mspec: Option<SessionTypesList>,
) -> OptWarnings<ast::CModule> {
    let mut warnings: Vec<String> = Vec::new();
    let mut functions: HashMap<Rc<FunName>, Vec<CFunClause>> = HashMap::new();
    let mut fallback_args: HashMap<Rc<FunName>, Vec<Var>> = HashMap::new();
    // One body for each module
    for fun_def in &ast.defs {
        let fname = &fun_def.name.inner; // TODO: Consider Rc at cerl parser level
        match compose_function_with_contract(&base_spec, &session_spec, fun_def) {
            Ok((fun_fallback_args, contract_clauses)) => {
                functions.insert(fname.clone(), contract_clauses);
                fallback_args.insert(fname.clone(), fun_fallback_args);
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
            mspec,
            functions,
            fallback_args,
        },
        cast_env: CastEnv(HashMap::new()),
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
) -> Result<(Vec<Var>, Vec<CFunClause>), String> {
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

    // If a function clause use a literal, then we need to use the top level binder instead.
    let fallback_binders_top: Vec<Var> = fun_def
        .body
        .fun
        .vars
        .clone()
        .into_iter()
        .map(|v| (*v.name).clone())
        .collect();

    // A top-level function expression may contain multiplte clauses.
    // If there are many clauses they will be contained within a normal case expression as the
    // first expression within the function expression body.
    // We check for this annotation. If the annotation is not present the function only has one
    // clause.
    let single_clause_std_args = fun_def
        .body
        .fun
        .vars
        .clone()
        .into_iter()
        .map(|v| CPat::Var((v.loc).clone(), Rc::new((*v.name).clone())))
        .collect();
    let clauses = match &fun_expr.body.inner.borrow() {
        Expr::Case(_location, _top_cases_var, top_cases_clauses) => {
            match &fun_expr.body.anno.0 {
                None => {
                    let cexpr = expr_to_cexpr(&fun_expr.body.inner)?;
                    vec![(single_clause_std_args, cexpr)]
                }
                Some(anno) => {
                    let [(_, anno)] = anno.as_slice() else {
                        todo!()
                    };
                    // Top level clause is a function if the annotation contains the function name:
                    if Const(Lit::Tuple(vec![
                        Lit::Atom(Atom("function".to_string()).into()),
                        Lit::Tuple(vec![
                            Lit::Atom(fname.name.clone()),
                            Lit::Int(fname.arity.try_into().unwrap()),
                        ]),
                    ])) == *anno
                    {
                        let mut clauses_res: Vec<(Vec<CPat>, CExpr)> = Vec::new();

                        // TODO: We throw away "when" right now. Include agian later. It will be needed for
                        // proper type checking later.
                        for clause in top_cases_clauses {
                            let clause_args = clause_pats_to_fun_header_args(&clause.inner.pats)?;
                            let cexpr = expr_to_cexpr(&clause.inner.res.inner)?;
                            clauses_res.push((clause_args, cexpr));
                        }
                        clauses_res
                    } else {
                        // Normal cases if not, gotta handle annotated normal cases.
                        todo!()
                    }
                }
            }
        }

        single_clause => match expr_to_cexpr(single_clause) {
            Ok(cexpr) => vec![(single_clause_std_args, cexpr)],
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
    for (partial_ctype_elm, (this_args, this_body)) in partial_cfun.iter().zip(clauses.iter()) {
        // It should be a very simple, "just add" stuff here
        contract_clauses.push(
            // TODO: Deduplicate args, but moving to function name does not seem like a good solution.
            CFunClause {
                spec: partial_ctype_elm.spec.clone(),
                args: this_args.clone(),
                body: this_body.clone(),
                return_type: partial_ctype_elm.return_type.clone(),
            },
        )
    }
    Ok((fallback_binders_top, contract_clauses))
}

fn clause_pats_to_fun_header_args(arg_pat: &Vec<AnnoPat>) -> Result<Vec<CPat>, String> {
    let mut res: Vec<CPat> = Vec::new();
    for elm in arg_pat {
        let elm_res = clause_pats_to_fun_header_arg(&elm.inner)?;
        res.push(elm_res);
    }
    Ok(res)
}

fn clause_pats_to_fun_header_arg(arg_pat: &Pat) -> Result<CPat, String> {
    match arg_pat {
        Pat::Var(loc, v) => Ok(CPat::Var(loc.clone(), v.name.clone())),
        Pat::Lit(loc, l) => Ok(CPat::Lit(loc.clone(), l.clone())),
        Pat::Tuple(loc, t) => {
            let mut tuple_content: Vec<CPat> = Vec::new();
            for elm in t {
                let res = clause_pats_to_fun_header_arg(&elm.inner)?;
                tuple_content.push(res);
            }
            Ok(CPat::Tuple(loc.clone(), tuple_content))
        }
        x => Err(format!(
            "when case is top-level function every arg must be a var. Found {}",
            x,
        )),
    }
}
// TODO: Can clause_to_cclause and expr_to_cexpr be "mered" into one name via some trait or
// similar?

fn clause_to_cclause(clause: &Clause) -> Result<CClause, String> {
    let res = expr_to_cexpr(&clause.res.inner)?;
    Ok(CClause {
        pats: clause
            .pats
            .clone()
            .into_iter()
            .map(|p| pat_to_cpat(&p.inner))
            .collect(),
        res,
    })
}

fn pat_to_cpat(pat: &Pat) -> CPat {
    match pat {
        Pat::Var(loc, v) => CPat::Var(loc.clone(), v.name.clone()),
        Pat::Lit(loc, l) => CPat::Lit(loc.clone(), l.clone()),
        Pat::Cons(loc, pat_cons) => CPat::Cons(
            loc.clone(),
            pat_cons.iter().map(|p| pat_to_cpat(&p.inner)).collect(),
        ),
        Pat::Tuple(loc, pat_tuple) => CPat::Tuple(
            loc.clone(),
            pat_tuple.iter().map(|p| pat_to_cpat(&p.inner)).collect(),
        ),
        Pat::Alias(loc, var, pat) => CPat::Alias(
            loc.clone(),
            var.name.clone(),
            pat_to_cpat(&pat.inner).into(),
        ),
    }
}

fn expr_to_cexpr(expr: &Expr) -> Result<CExpr, String> {
    match expr {
        Expr::Var(loc, v) => Ok(CExpr::Var(loc.clone(), v.clone())),
        Expr::AtomLit(loc, l) => Ok(CExpr::Lit(loc.clone(), l.clone())),
        Expr::Cons(loc, exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = expr_to_cexpr(&expr.inner)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Cons(loc.clone(), tuple))
        }
        Expr::Exprs(loc, exprs) => {
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
                Ok(CExpr::Tuple(loc.clone(), tuple))
            }
        }
        Expr::Tuple(loc, exprs) => {
            let mut tuple: Vec<CExpr> = Vec::new();
            for expr in exprs {
                let cexpr = expr_to_cexpr(&expr.inner)?;
                tuple.push(cexpr)
            }
            Ok(CExpr::Tuple(loc.clone(), tuple))
        }
        Expr::Let(loc, v, e1, e2) => {
            let v = match v.as_slice() {
                [v] => Rc::new(CPat::Var(v.loc.clone(), v.name.clone())),
                // TODO: The Location for the CPat::Tuple below is wrong, the Loc for the whole Let
                // is reused. Change if needed, requires to save one more location in the parser.
                _ => Rc::new(CPat::Tuple(
                    loc.clone(),
                    v.iter()
                        .map(|v| CPat::Var(v.loc.clone(), v.name.clone()))
                        .collect(),
                )),
            };
            let e1 = expr_to_cexpr(&e1.inner)?.into();
            let e2 = expr_to_cexpr(&e2.inner)?.into();

            Ok(CExpr::Let(loc.clone(), v.clone(), e1, e2))
        }
        Expr::Case(loc, e1, e2) => {
            let base_expr = expr_to_cexpr(&e1.inner)?.into();
            let mut contract_clauses: Vec<CClause> = Vec::new();
            for clause in e2 {
                match clause.anno.borrow() {
                    Anno(Some(clause_anno)) => {
                        match clause_anno.as_slice() {
                            [(_, Const(Lit::Atom(clause_anno)))]
                                if clause_anno.0 == "compiler_generated" =>
                            {
                                println!("Warning: Skipped compiler generated case."); // TODO: Proper warning
                                continue;
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                let res = clause_to_cclause(&clause.inner)?;
                contract_clauses.push(res);
            }
            Ok(CExpr::Case(loc.clone(), base_expr, contract_clauses))
        }
        Expr::Call(loc, fun_module, fun_call, args) => {
            let mut args_cexpr: Vec<CExpr> = Vec::new();
            for arg in args {
                let cexpr = expr_to_cexpr(&arg.inner)?;
                args_cexpr.push(cexpr);
            }

            match (*fun_call.inner).borrow() {
                Expr::AtomLit(_, fun_call) => {
                    let Lit::Atom(fun_call) = fun_call.borrow() else {
                        return Err("Unsupported call name type.".to_string());
                    };
                    match fun_module {
                        CallModule::PrimOp => {
                            Ok(CExpr::PrimOp(loc.clone(), fun_call.clone(), args_cexpr))
                        }
                        CallModule::Call(module_name_call) => {
                            let Expr::AtomLit(_loc, module_name_call) =
                                module_name_call.inner.borrow()
                            else {
                                return Err("Unsupported module name type".to_string());
                            };
                            let Lit::Atom(module_name_call) = module_name_call.borrow() else {
                                return Err("Unsupported module name type".to_string());
                            };
                            Ok(CExpr::Call(
                                loc.clone(),
                                module_name_call.clone(),
                                fun_call.clone(),
                                args_cexpr,
                            ))
                        }
                        _ => Err("Function call arguments has unsupported types.".to_string()),
                    }
                }
                Expr::Fname(_, fname) => {
                    if *fun_module != CallModule::Apply {
                        return Err("Unsupported call type.".to_string());
                    }
                    Ok(CExpr::Apply(loc.clone(), fname.clone(), args_cexpr))
                }
                _ => Err("Unsupported call name type.".to_string()),
            }
        }
        Expr::Do(loc, e1, e2) => {
            let e1 = expr_to_cexpr(&e1.inner)?.into();
            let e2 = expr_to_cexpr(&e2.inner)?.into();
            Ok(CExpr::Do(loc.clone(), e1, e2))
        }
        _ => Err("Expression not supported in Contract Core Erlang yet.".to_string()),
    }
}
