mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;
mod type_checker;

use crate::contract_cerl::compose_contract::compose_contract;
use cerl_parser::{ast::Module, top::module};
use contract_cerl::ast::{CModule, OptWarnings};
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use miette::{NamedSource, Result};
use nom_supreme::error::GenericErrorTree::Alt;
use nom_supreme::final_parser::{Location, RecreateContext};
use nom_supreme::{error::ErrorTree, final_parser::final_parser};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(
    code(parse::error),
    // url(docsrs),
    // help("try doing it better next time?")
)]
struct ParseError {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("here")]
    bad_bit: SourceSpan,
    #[help]
    advice: Option<String>,
}

fn cerl_final(input: &str) -> Result<Module, ErrorTree<&str>> {
    final_parser(module)(input)
}

pub fn parse<'a>(filename: &str, src: &'a str) -> Result<OptWarnings<CModule>> {
    let module = cerl_final(src);
    match module {
        Err(err) => {
            // https://docs.rs/nom-supreme/latest/nom_supreme/error/enum.GenericErrorTree.html
            // dbg!(err) is a usefull marco
            match err {
                Alt(err_vec) => {
                    for elm in err_vec {
                        match elm {
                            nom_supreme::error::GenericErrorTree::Base { location, kind } => {
                                let location = Location::recreate_context(src, location);
                                let src = src.to_string();
                                let len = src.len();

                                Err(ParseError {
                                    src: NamedSource::new(filename, src),
                                    bad_bit: (location.line, location.column).into(),
                                    advice: Some(format!("{:?}", kind)),
                                })?;
                                println!("Error in {}", filename);
                                //println!("{:?}", Location::recreate_context(src, location));
                                println!("Kind: {:?}", kind);
                            }
                            other => {
                                todo!(
                                    "Pretty error message not implemented. Raw error: {:?}",
                                    other
                                );
                            }
                        }
                    }
                    todo!("Should return error in a 'pretty' way");
                }
                other => {
                    todo!(
                        "Pretty error message not implemented. Raw error: {:?}",
                        other
                    );
                }
            }
        }
        Ok(module) => Ok(compose_contract(module).unwrap()),
    }
}

pub fn type_check(m: CModule) -> OptWarnings<bool> {
    crate::type_checker::module::module(m)
}

// pub fn analyze(m: Module) -> bool {
//     let (env,skipped) = init functions env(m);
//     analyze_module_functions(&env, &skipped)
// }
