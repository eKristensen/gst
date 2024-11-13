use std::{borrow::BorrowMut, rc::Rc};

use crate::{
    cerl_parser::ast::{
        Anno, AnnoClause, AnnoExpr, AnnoFun, AnnoModule, AnnoPat, AnnoVar, Atom, CLoc, CallModule,
        Clause, Expr, Fun, FunDef, Lit, Loc, Module, Pat, Var,
    },
    contract_cerl::{
        ast::{CExpr, CType},
        types::BaseType,
    },
};

use super::env::{Cast, CastEnv};

pub fn add_gradual_cast(
    cast_env: &mut CastEnv,
    cloc: &Rc<CLoc>,
    input: &CType,
    output: &CType,
) -> Result<(), String> {
    match (input, output) {
        (CType::Base(BaseType::Dynamic), CType::Base(BaseType::Integer)) => {
            if cast_env.0.contains_key(cloc) {
                return Err(format!(
                    "Cast already inserted for location. Should not happen"
                ));
            }
            cast_env.0.insert(cloc.clone(), Cast::DynToInt);
            Ok(())
        }
        _ => Err(format!(
            "Could not find any possbile cast for given input/output type",
        )),
    }
}

// AnnoModule is always the top-level so it gets no special name.
pub fn cast_insertion(cast_env: &CastEnv, e: &AnnoModule) -> AnnoModule {
    // No insertion at top level
    AnnoModule {
        inner: cast_insertion_module(cast_env, &e.inner),
        ..e.clone()
    }
}

fn cast_insertion_module(cast_env: &CastEnv, e: &Module) -> Module {
    // No insertion at top level
    // Only expect cast insertion in defs
    Module {
        defs: e
            .defs
            .iter()
            .map(|def| cast_insertion_defs(cast_env, def))
            .collect(),
        ..e.clone()
    }
}

fn cast_insertion_defs(cast_env: &CastEnv, e: &FunDef) -> FunDef {
    // No insertion at top level
    // Only expect cast insertion in def body
    FunDef {
        body: Rc::new(AnnoFun {
            fun: cast_insertion_fun(cast_env, &e.body.fun).into(),
            ..(*e.body).clone()
        }),
        ..e.clone()
    }
}

fn cast_insertion_fun(cast_env: &CastEnv, e: &Fun) -> Fun {
    // We again assume only to replace in the body of the function. No checking for loaction yet
    Fun {
        body: cast_insertion_anno_expr(cast_env, &e.body).into(),
        ..e.clone()
    }
}

fn cast_insertion_anno_expr(cast_env: &CastEnv, e: &AnnoExpr) -> AnnoExpr {
    // We again assume only to replace in the body of the function. No checking for loaction yet
    AnnoExpr {
        inner: cast_insertion_expr(cast_env, &e.inner).into(),
        ..e.clone()
    }
}

fn cast_insertion_expr(cast_env: &CastEnv, e: &Expr) -> Expr {
    // Here we need to check for location.
    match e {
        // For now assume cast insertion always happens on a variable and never on any other kind
        // of expression
        Expr::Var(loc, v) if cast_env.0.contains_key(loc) => {
            println!("INFO: Cast code injection");
            cast_code_injection(cast_env.0.get(loc).unwrap(), v)
        }
        Expr::Call(loc, module, name, args) => Expr::Call(
            loc.clone(),
            module.clone(),
            name.clone(),
            args.iter()
                .map(|arg| cast_insertion_anno_expr(cast_env, arg))
                .collect(),
        ),
        Expr::Let(loc, v, e1, e2) => Expr::Let(
            loc.clone(),
            v.clone(), // TODO: We do not check for var to insert cast here
            cast_insertion_anno_expr(cast_env, e1).into(),
            cast_insertion_anno_expr(cast_env, e2).into(),
        ),
        // Otherwise just clone it and keep it unchanged.
        e => e.clone(),
    }
}

fn cast_code_injection(cast: &Cast, var: &Rc<Var>) -> Expr {
    // TODO: Annotation could be like "gst type checker inserted" or something like that
    // Dummy location
    let dloc: Rc<CLoc> = CLoc {
        comment: None,
        start: Loc { line: 0, column: 0 },
        end: Loc { line: 0, column: 0 },
    }
    .into();
    if *cast == Cast::DynToInt {
        Expr::Case(
            // TODO: Maybe location should be optional to avoid stupid insertions like these?
            dloc.clone(),
            AnnoExpr {
                loc: dloc.clone(),
                anno: Anno(None).into(),
                inner: Expr::Var(dloc.clone(), var.clone()).into(),
            }
            .into(),
            vec![
                // Check integertype at runtime
                AnnoClause {
                    loc: dloc.clone(),
                    anno: Anno(None).into(),
                    inner: Clause {
                        loc: dloc.clone(),
                        pats: vec![AnnoPat {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Pat::Var(
                                dloc.clone(),
                                AnnoVar {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    name: var.clone(),
                                }
                                .into(),
                            )
                            .into(),
                        }],
                        // TL;DR: call erlang:is_integer(var) in when
                        when: AnnoExpr {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Expr::Call(
                                dloc.clone(),
                                CallModule::Call(
                                    AnnoExpr {
                                        loc: dloc.clone(),
                                        anno: Anno(None).into(),
                                        inner: Expr::AtomLit(
                                            dloc.clone(),
                                            Lit::Atom(Atom("erlang".to_string()).into()).into(),
                                        )
                                        .into(),
                                    }
                                    .into(),
                                ),
                                AnnoExpr {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    inner: Expr::AtomLit(
                                        dloc.clone(),
                                        Lit::Atom(Atom("is_integer".to_string()).into()).into(),
                                    )
                                    .into(),
                                }
                                .into(),
                                vec![AnnoExpr {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    inner: Expr::Var(dloc.clone(), var.clone()).into(),
                                }],
                            )
                            .into(),
                        }
                        .into(),
                        res: AnnoExpr {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Expr::Var(dloc.clone(), var.clone()).into(),
                        }
                        .into(),
                    }
                    .into(),
                },
                // Fallback if type does not match
                AnnoClause {
                    loc: dloc.clone(),
                    anno: Anno(None).into(),
                    inner: Clause {
                        loc: dloc.clone(),
                        pats: vec![AnnoPat {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Pat::Var(
                                dloc.clone(),
                                AnnoVar {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    name: var.clone(),
                                }
                                .into(),
                            )
                            .into(),
                        }],
                        when: AnnoExpr {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Expr::AtomLit(
                                dloc.clone(),
                                Lit::Atom(Atom("true".to_string()).into()).into(),
                            )
                            .into(),
                        }
                        .into(),
                        res: AnnoExpr {
                            loc: dloc.clone(),
                            anno: Anno(None).into(),
                            inner: Expr::Call(
                                dloc.clone(),
                                CallModule::Call(
                                    AnnoExpr {
                                        loc: dloc.clone(),
                                        anno: Anno(None).into(),
                                        inner: Expr::AtomLit(
                                            dloc.clone(),
                                            Lit::Atom(Atom("erlang".to_string()).into()).into(),
                                        )
                                        .into(),
                                    }
                                    .into(),
                                ),
                                AnnoExpr {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    inner: Expr::AtomLit(
                                        dloc.clone(),
                                        Lit::Atom(Atom("error".to_string()).into()).into(),
                                    )
                                    .into(),
                                }
                                .into(),
                                vec![AnnoExpr {
                                    loc: dloc.clone(),
                                    anno: Anno(None).into(),
                                    inner: Expr::AtomLit(
                                        dloc.clone(),
                                        Lit::Atom(Atom("gst_bad_cast".to_string()).into()).into(),
                                    )
                                    .into(),
                                }],
                            )
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                },
            ],
        )
    } else {
        todo!("Not yet supported");
    }
}

// TODO: Would a Loc "wrapper"" like Anno in Cerl AST make sense here instead?
pub fn get_cexpr_loc(e: &CExpr) -> Rc<CLoc> {
    match e {
        CExpr::Var(loc, _) => loc.clone(),
        CExpr::Lit(loc, _) => loc.clone(),
        CExpr::Cons(loc, _) => loc.clone(),
        CExpr::Tuple(loc, _) => loc.clone(),
        CExpr::Let(loc, _, _, _) => loc.clone(),
        CExpr::Case(loc, _, _) => loc.clone(),
        CExpr::PrimOp(loc, _, _) => loc.clone(),
        CExpr::Apply(loc, _, _) => loc.clone(),
        CExpr::Call(loc, _, _, _) => loc.clone(),
        CExpr::Do(loc, _, _) => loc.clone(),
        CExpr::Fun(loc, _, _) => loc.clone(),
        CExpr::ApplyFun(loc, _, _) => loc.clone(),
    }
}
