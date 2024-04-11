// Purpose of this file is to check for elements that are mostly syntax, but seems to fit better at a slightly higher abstraction level.
// They could for the most case be included when parsing with nom, but splitting responsibilities makes it easer to read, write and maintain the source code at the expense that well formed checks could be "forgotten".
// However the data-structure does allows to store data that makes no sense so using this for sanity check makes sense in any case.

use crate::{
    cerl_parser::ast::Module,
    st_parser::ast::{SessionElement, SessionElementList},
};

use super::env::Funcs;

// TODO high and low level test of check_wf_st_t

// cerl parsed module
// init_env includes parsed session types
pub fn check_wf(_: Module, env: &Funcs) -> Result<(), String> {
    for elm in env.values() {
        for arg in &elm.contract {
            // There is something to check when contract is new or ongoing.
            // Check no ST has elements after "end."
            match arg {
                super::env::FunContract::Base(_) => continue, // Nothing to check with base type
                super::env::FunContract::New(st) => check_wf_st_elm(st)?,
                super::env::FunContract::Ongoing(st) => check_wf_st_elm(st)?,
            }

                // TODO
            // Check all binders refer to variables that are defined in the function body.
            // Remember to exclude function argument variables... (those names are automatically generated and should not be used)
            //check_binder_body(&elm.body, &session.binders)?;
        }
    }

    // TODO: keywords check? Maybe it would sense if labels could not be named "new" or "ongoing"

    Ok(())
}

fn check_wf_st_elm(st: &SessionElementList) -> Result<(), String> {
    let mut end_seen = false;
    let SessionElementList(st) = st;
    for elm in st {
        if end_seen {
            return Err(format!("Saw end before {:?}", elm));
        }
        match elm {
            SessionElement::Send(_) => (),
            SessionElement::Receive(_) => (),
            SessionElement::MakeChoice(_, mc_st) => check_wf_st_elm(mc_st)?,
            SessionElement::OfferChoice(oc) => {
                for oc_em in oc.values() {
                    check_wf_st_elm(oc_em)?
                }
            }
            SessionElement::End => {
                end_seen = true;
            }
        }
    }

    Ok(())
}

// fn check_binder_body(
//     body: &Vec<(FunHead, FunDef)>,
//     binders: &HashMap<Var, Vec<SessionElement>>,
// ) -> Result<(), String> {
//     let mut used_vars: HashSet<Var> = HashSet::new(); // All vars that are allowed to be in binders
//     let mut excluded_vars: HashSet<Var> = HashSet::new(); // Argument wars are banned
//     for (_, elm) in body {
//         // Remember variables to never include
//         for arg in elm.args {
//             // TODO there must be a smarter way to insert stuff into the excluded_vars set
//             excluded_vars.insert(arg.clone());
//         }

//         extract_used_vars_exprs(&mut used_vars, &elm.body);
//     }
//     Ok(())
// }

// fn extract_used_vars_exprs(
//     used_vars: &mut HashSet<Var>,
//     exprs: &Exprs
// ) -> () {
//     match exprs {
//         Exprs::Single(expr) => extract_used_vars_expr(used_vars, expr),
//         Exprs::Values(_) => todo!("never seen values used yet..."),
//     }
// }

// Not as trivial as I expected. cons and tuple needs to be handled somehow. Even nested...
// fn extract_used_vars_expr(
//     used_vars: &mut HashSet<Var>,
//     expr: &Expr
// ) -> () {
//     match expr {
//         Expr::Var(_) => (),
//         Expr::Fname(_) => (),
//         Expr::Lit(_) => (),
//         Expr::Fun(_) => (),
//         Expr::Cons(_) => todo!(),
//         Expr::Tuple(_) => todo!(),
//         Expr::Let(_, _, _) => todo!(),
//         Expr::Case(_, _) => todo!(),
//         Expr::LetRec(_, _) => todo!(),
//         Expr::Apply(_, _) => todo!(),
//         Expr::Call(_, _, _) => todo!(),
//         Expr::PrimOp(_, _) => todo!(),
//         Expr::Receive(_, _, _) => todo!(),
//         Expr::Try(_, _, _, _, _) => todo!(),
//         Expr::Do(_, _) => todo!(),
//         Expr::Catch(_) => todo!(),
//         Expr::Map(_, _) => todo!(),
//     }
// }
