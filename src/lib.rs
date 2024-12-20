mod cerl_parser;
mod contract_cerl;
mod spec_extractor;
mod st_parser;
mod type_checker;

use crate::contract_cerl::compose_contract::compose_contract;
use cerl_parser::ast::AnnoModule;
use cerl_parser::cinput::CInput;
use cerl_parser::grammar::run_parser;
use contract_cerl::ast::{CModule, OptWarnings};
use miette::{Diagnostic, SourceSpan};
use miette::{NamedSource, Result};
use nom_supreme::error::GenericErrorTree;
use nom_supreme::final_parser::{Location, RecreateContext};
use nom_supreme::{error::ErrorTree, final_parser::final_parser};
use thiserror::Error;
use type_checker::env::CastEnv;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(
    code(parse::error),
    // url(docsrs),
    // help("try doing it better next time?")
)]
struct ParseError {
    // Snippets and highlights can be included in the diagnostic!
    #[label("here")]
    bad_bit: SourceSpan,
    #[help]
    advice: Option<String>,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Parser Error. See help below.")]
#[diagnostic()]
struct ParseErrors {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    #[related]
    related: Vec<ParseError>,
}

// TODO: This function should not be public
pub fn sanity_check_final_parser(src: &str) -> Result<AnnoModule, ErrorTree<CInput>> {
    cerl_final(CInput::new(src))
}

fn cerl_final(input: CInput) -> Result<AnnoModule, ErrorTree<CInput>> {
    final_parser(run_parser)(input)
}

// TODO: More neat way to also preserve "pure" AST for .core files
pub fn parse(filename: &str, src: &str) -> Result<(AnnoModule, OptWarnings<CModule>)> {
    let module = cerl_final(CInput::new(src));
    match module {
        Err(err) => {
            // https://docs.rs/nom-supreme/latest/nom_supreme/error/enum.GenericErrorTree.html
            // dbg!(err) is a usefull marco
            match err {
                GenericErrorTree::Alt(err_vec) => {
                    let mut err_msgs: Vec<ParseError> = Vec::new();
                    for elm in err_vec {
                        match elm {
                            GenericErrorTree::Base { location, kind } => {
                                let location = Location::recreate_context(src, location.input);

                                err_msgs.push(ParseError {
                                    bad_bit: (location.line, location.column).into(),
                                    advice: Some(format!("{:?}", kind)),
                                });
                            }
                            GenericErrorTree::Stack { base, contexts } => {
                                for (one_str, stack) in contexts {
                                    println!("Str: {:?}", one_str);
                                    println!("Stack: {:?}", stack);
                                }
                                dbg!(base);
                            }
                            other => {
                                todo!(
                                    "Pretty error message not implemented #2. Raw error: {:?}",
                                    other
                                );
                            }
                        }
                    }

                    // Based on MultiError example from https://docs.rs/miette/latest/miette/index.html#example
                    Err(ParseErrors {
                        src: NamedSource::new(filename, src.to_string()),
                        related: err_msgs,
                    })?;
                    todo!("Should return error in a 'pretty' way");
                }
                other => {
                    todo!(
                        "Pretty error message not implemented #1. Raw error: {:?}",
                        other
                    );
                }
            }
        }
        // TODO: Avoid clone here?
        Ok(module) => Ok((module.clone(), compose_contract(module).unwrap())),
    }
}

pub fn type_check(m: CModule) -> OptWarnings<bool> {
    crate::type_checker::module::module(m)
}

pub fn cast_insertion(cast_env: &CastEnv, e: &AnnoModule) -> AnnoModule {
    type_checker::casting::cast_insertion(cast_env, e)
}

// pub fn analyze(m: Module) -> bool {
//     let (env,skipped) = init functions env(m);
//     analyze_module_functions(&env, &skipped)
// }
