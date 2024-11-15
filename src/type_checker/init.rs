use std::rc::Rc;

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
    for (clause_var, (var_ctype, top_var)) in args.iter().zip(spec.iter().zip(fallback.iter())) {
        // TODO: Not binding to top-level binder might be a problem... But not so far. Bind to top
        // level as a fallback.
        pat_init_envs(envs, clause_var, Some(&(top_var.clone()).into()), var_ctype)?;
    }
    Ok(())
}

fn pat_init_envs(
    envs: &mut TypeEnvs,
    clause_var: &CPat,
    top_var: Option<&Var>,
    var_ctype: &CType,
) -> Result<(), String> {
    // Always bind top_var if given
    // NOTE: Assumption: Top var is never composed, i.e. always CPat::Var
    if let Some(top_var) = top_var {
        init_add_var_ctype_envs(envs, &((top_var.clone()).into()), var_ctype)?;
    }

    // "Normal"" var binding.
    match clause_var {
        CPat::Var(_, var) => init_add_var_ctype_envs(envs, var, var_ctype),
        CPat::Tuple(_, t) => {
            // NOTE: Interesting case:
            // sub bind: [Lit(Atom(Atom("add_1"))), Var(Var("V1"))] Base(Tuple([Atom(Atom("add_1")), Integer]))

            // NOTE: Either we have a Dynamic type or a specific type we need to match. The dynamic type is very permissive and we need to save all possible variables to properly typecheck the code
            if *var_ctype == CType::Base(BaseType::Dynamic) {
                // Dynamic for the win
                for elm in t {
                    pat_init_envs(envs, elm, None, var_ctype)?;
                }
                Ok(())
            } else {
                // Or precision if possible.
                // Here we also need to match the CType
                if let CType::Base(BaseType::Tuple(ctypes)) = var_ctype {
                    if t.len() != ctypes.len() {
                        panic!("Mismatch tuple length");
                    }
                    for (inner_var, inner_ctype) in t.iter().zip(ctypes) {
                        pat_init_envs(envs, inner_var, None, &CType::Base(inner_ctype.clone()))?;
                    }
                    Ok(())
                } else {
                    panic!("Mismatch on ctype and cexpr");
                }
            }
        }
        CPat::Lit(_, _) => Ok(()), // NOTE: Maybe throwing useful info away here...
        x => todo!("pat_init_envs missing impl for: {:?}", x),
    }
}

fn init_add_var_ctype_envs(
    envs: &mut TypeEnvs,
    var: &Rc<Var>,
    ctype: &CType,
) -> Result<(), String> {
    let insert_var_env = match ctype {
        CType::Base(base_type) => Sigma(base_type.clone()),
        CType::New(session_type) => Gamma(session_type.clone()),
        CType::Consume(session_type) => Delta(session_type.clone()),
    };
    if envs.0.insert(var.clone(), insert_var_env.clone()).is_some() {
        // Duplicates might happen if the function header and clause has the same var names. Allow
        // if there are no conflicts
        if insert_var_env == *envs.0.get(var).unwrap() {
            Ok(())
        } else {
            Err(format!(
                "Duplicate var {:?} in args, should not be possible!!!",
                var
            ))
        }
    } else {
        Ok(())
    }
}
