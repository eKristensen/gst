use crate::contract_cerl::ast::{CExpr, CModule, CType};

use super::env::TypeEnvs;

pub fn expr(module: &CModule, envs: &TypeEnvs, expr: &CExpr) -> CType {
    match expr {
        CExpr::Var(_) => todo!(),
        CExpr::Lit(_) => todo!(),
        CExpr::Cons(_) => todo!(),
        CExpr::Tuple(_) => todo!(),
        CExpr::Case(_, _) => todo!(),
        CExpr::Call(_, _) => todo!(),
    }
}
