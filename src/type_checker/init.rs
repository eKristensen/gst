use crate::contract_cerl::ast::CPat;
use crate::contract_cerl::types::BaseType;
use crate::{cerl_parser::ast::Var, contract_cerl::ast::CType};

use super::env::TypeEnvs;
use crate::type_checker::env::TypeEnv::Delta;
use crate::type_checker::env::TypeEnv::Gamma;
use crate::type_checker::env::TypeEnv::Sigma;

pub fn init_env(
    envs: &mut TypeEnvs,
    args: &[CPat],
    spec: &[CType],
    fallback: &[Var],
) -> Result<(), String> {
    // TODO: For all places .zip is used, length should be checked.
    // Rust makes no errors, just stops when size is mismatched, see
    // https://stackoverflow.com/questions/57345197/
    if args.len() != spec.len() {
        // Panic is acceptable as this should have been checked
        // before contract core erlang was constructed
        // Panic is better than silent acceptance
        return Err("args and spec do not have the same length".to_string());
    }
    for (var, (elm_ctype, elm_fallback)) in args.iter().zip(spec.iter().zip(fallback.iter())) {
        // TODO: Not binding to top-level binder might be a problem... But not so far. Bind to top
        // level as a fallback.
        let var = match var {
            CPat::Var(var) => var,
            CPat::Tuple(t) => {
                // Try to bind sub elements
                println!("sub bind: {:?} {:?}", t, elm_ctype);
                // Interesting case:
                // sub bind: [Lit(Atom(Atom("add_1"))), Var(Var("V1"))] Base(Tuple([Atom(Atom("add_1")), Integer]))
                pat_init_envs(envs, t, elm_ctype);
                elm_fallback
            }
            _ => elm_fallback,
        };
        let insert_res = match elm_ctype {
            CType::Base(base_type) => envs
                .0
                .insert(var.clone().into(), Sigma(base_type.clone()))
                .is_none(),
            CType::New(session_type) => envs
                .0
                .insert(var.clone().into(), Gamma(session_type.clone()))
                .is_none(),
            CType::Consume(session_type) => envs
                .0
                .insert(var.clone().into(), Delta(session_type.clone()))
                .is_none(),
        };
        if !insert_res {
            return Err("Duplicate var in args, should not be possible!!!".to_string());
        }
    }
    Ok(())
}

// TODO: More generic than just tuples...
// TODO: This function needs to be recursive somehow. Maybe be inspired by type checker pattern
// matching?
fn pat_init_envs(envs: &mut TypeEnvs, right: &[CPat], left: &CType) {
    // Unpack assuming tuple
    let CType::Base(left) = left else { return };
    let BaseType::Tuple(left) = left else {
        return;
    };
    for (elm_left, elm_right) in right.iter().zip(left.iter()) {
        match (elm_left, elm_right) {
            (CPat::Var(var), elm_right) => {
                let insert_res = envs
                    .0
                    .insert(var.clone(), Sigma(elm_right.clone()))
                    .is_none();

                if !insert_res {
                    todo!("Duplicate var in args, should not be possible!!!");
                }
            }
            _ => continue,
        }
    }
}
