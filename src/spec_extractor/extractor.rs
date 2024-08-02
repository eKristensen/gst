// Purpose: take in Core Erlang AST and output usable base spec type specification

use std::collections::HashMap;

use crate::{
    cerl_parser::{
        self,
        ast::{Atom, FunName, Lit},
    },
    contract_cerl::types::BaseType,
};

use super::ast::{BaseSpec, BaseSpecDef, BaseSpecElm, BaseSpecs};

pub fn base_spec_extractor(ast: &cerl_parser::ast::Module) -> Result<BaseSpecDef, String> {
    let mut base_spec_def: BaseSpecDef = BaseSpecDef(HashMap::new());

    // Use attributes to get spec
    for attribute in &ast.attributes {
        let Atom(a_name) = &attribute.name.name;
        if *a_name == "spec" {
            match add_base_spec(&attribute.value.inner) {
                Ok((fun_name, base_specs)) => {
                    if base_spec_def.0.contains_key(&fun_name) {
                        return Err(format!(
                            "FATAL: Duplicate -session for function {}. Exiting now...",
                            &fun_name
                        ));
                    } else {
                        base_spec_def.0.insert(fun_name.clone(), base_specs);
                    }
                }
                Err(err_val) => {
                    return Err(format!("Failed base spec extraction. Reason: {}", err_val))
                }
            }
        }
        // TODO: Add support for custom type declarations?
    }
    Ok(base_spec_def)
}

fn add_base_spec(spec: &Lit) -> Result<(FunName, BaseSpecs), String> {
    // Alright I have it with a simple spec
    // But how does it look when spec contains ; ???
    let (fname, spec) = get_fname_content(spec)?;
    let absform_clauses = get_absform_clauses(spec)?;
    let mut base_specs: BaseSpecs = BaseSpecs(Vec::new());
    for absform_clause in absform_clauses {
        let (input, output) = get_absform_clause(absform_clause)?;
        let input = get_absform_types(&input)?;
        let BaseSpecElm::Base(output) = get_absform_type(&output)? else {
            return Err("add_base_spec: Output type must be a base type".to_string());
        };
        base_specs.0.push(BaseSpec {
            args: input,
            return_type: output,
        })
    }
    Ok((fname, base_specs))
}

// Recognize pattern for spec fname.
fn get_fname_content(spec: &Lit) -> Result<(FunName, &Lit), String> {
    // Outer wrapper is a list with a two-element tuple
    let Lit::Cons(spec) = spec else {
        return Err("get_fname_content expected outer cons".to_string());
    };
    let &[spec] = &spec.as_slice() else {
        return Err("get_fname_content expected single element".to_string());
    };

    let Lit::Tuple(spec) = spec else {
        return Err("get_fname_content: Main tuple missing".to_string());
    };
    // The main spec tuple
    // The tuple has two elements: Function name encoded as a two-element tuple and the inner content.
    let &[fname_tuple, inner_spec] = &spec.as_slice() else {
        return Err("get_fname_content: Main tuple wrong format".to_string());
    };

    let Lit::Tuple(fname_tuple) = fname_tuple else {
        return Err("get_fname_content: Fun-name must be in a tuple".to_string());
    };
    let &[Lit::Atom(fun_name), Lit::Int(arity)] = &fname_tuple.as_slice() else {
        return Err("get_fname_content: Wrong fun name tuple format".to_string());
    };

    // Sanity check
    if *arity < 0 {
        return Err("get_fname_content: negative arity is bad".to_string());
    }

    // Unsigned arity
    let arity = usize::try_from(*arity);
    if arity.is_err() {
        return Err("get_fname_content: Arity conversion to unsigned failed".to_string());
    }
    let arity = arity.unwrap();

    Ok((
        FunName {
            name: (*fun_name).clone(),
            arity,
        },
        inner_spec,
    ))
}

fn get_absform_clauses(spec: &Lit) -> Result<&Vec<Lit>, String> {
    // Expect top-level list and split each element. Very simple
    let Lit::Cons(spec) = spec else {
        return Err("get_absform_clauses: -spec parse failure #1".to_string());
    };
    Ok(spec)
}

fn get_absform_clause(spec: &Lit) -> Result<(Lit, Lit), String> {
    // Each clause starts with a certain type fun tag we need to check exists, and otherwise ignore
    // We check, but ignore the location info that is encoded in the the absform.
    let Lit::Tuple(spec) = spec else {
        return Err("get_absform_clause must be in a tuple".to_string());
    };

    let &[Lit::Atom(tag), _, Lit::Atom(tag2), Lit::Cons(io)] = &spec.as_slice() else {
        return Err("get_absform_clause: Wrong length tuple.".to_string());
    };
    if *tag != Atom("type".to_owned()) || *tag2 != Atom("fun".to_owned()) {
        return Err("get_absform_clause wrong tags in tuple".to_string());
    }

    if io.len() != 2 {
        return Err("get_absform_clause: -spec parse failure #10".to_string());
    }

    // We output input and output of the clause ready to be parsed later
    Ok((io[0].clone(), io[1].clone()))
}

fn get_absform_types(spec_in: &Lit) -> Result<Vec<BaseSpecElm>, String> {
    // Product type is a special kind of type that can nest other types.
    let Lit::Tuple(spec) = spec_in else {
        return Err("get_absform_product_type can only be in a tuple.".to_string());
    };

    // TODO: Not ignore Anno/check content of annotation
    let &[Lit::Atom(tag), _, Lit::Atom(tag2), Lit::Cons(types_list)] = &spec.as_slice() else {
        return Err("get_absform_types: Wrong length tuple.".to_string());
    };
    println!("Debug how it looks here: {}", spec_in);
    if *tag == Atom("type".to_owned()) && *tag2 != Atom("product".to_owned()) {
        // Not product type assume a it is a ordinary type instead
        let single_type = get_absform_type(spec_in)?;
        return Ok(vec![single_type]);
    }

    // Each of these types should be parsed:
    let mut base_types_out: Vec<BaseSpecElm> = Vec::new();
    for elm in types_list {
        let elm_type = get_absform_type(elm)?;
        base_types_out.push(elm_type);
    }

    Ok(base_types_out)
}

fn get_absform_type(spec: &Lit) -> Result<BaseSpecElm, String> {
    // Expecting a tuple
    let Lit::Tuple(spec) = spec else {
        return Err(format!("get_absform_type expected tuple but got {}", spec));
    };

    if spec.is_empty() {
        return Err("get_absform_type cannot work with empty type tuple".to_string());
    }

    // Type matching
    // TODO: Not ignore Anno
    match &spec.as_slice() {
        &[Lit::Atom(tag), _, Lit::Atom(tag2), Lit::Nil] => {
            match (tag.0.as_str(), tag2.0.as_str()) {
                ("user_type", "new") => Ok(BaseSpecElm::New),
                ("user_type", "consume") => Ok(BaseSpecElm::Consume),
                ("type", "pid") => Ok(BaseSpecElm::Base(BaseType::Pid)),
                ("type", "reference") => Ok(BaseSpecElm::Base(BaseType::Reference)),
                ("type", "integer") => Ok(BaseSpecElm::Base(BaseType::Integer)),
                ("type", "float") => Ok(BaseSpecElm::Base(BaseType::Float)),
                ("type", "boolean") => Ok(BaseSpecElm::Base(BaseType::Boolean)),
                ("type", "list") => Ok(BaseSpecElm::Base(BaseType::List)),
                ("type", "string") => Ok(BaseSpecElm::Base(BaseType::String)),
                _ => Err("Could not match any type".to_string()),
            }
        }

        &[Lit::Atom(tag), _, Lit::Atom(tag2), Lit::Cons(args)] => {
            match (tag.0.as_str(), tag2.0.as_str(), args.as_slice()) {
                ("type", type_name, type_args) => Err(format!(
                    "Unknown type {:?} with args {:?}",
                    type_name, type_args
                )),
                ("user_type", type_name, type_args) => Err(format!(
                    "Unknown user_type {:?} with args {:?}",
                    type_name, type_args
                )),
                _ => Err("Could not match any type".to_string()),
            }
        }
        &[Lit::Atom(tag), _, Lit::Atom(tag2), Lit::Atom(tag3)] => {
            match (tag.0.as_str(), tag2.0.as_str(), tag3.0.as_str()) {
                ("type", "map", "any") => Ok(BaseSpecElm::Base(BaseType::Map)),
                ("type", "tuple", "any") => {
                    Ok(BaseSpecElm::Base(BaseType::Tuple(vec![BaseType::Term])))
                }
                x => Err(format!(
                    "Unknown type where map or tuple was expected: {:?}",
                    x
                )),
            }
        }
        &[Lit::Atom(tag), _, Lit::Atom(tag2)] => match (tag.0.as_str(), tag2) {
            ("atom", atom_name) => Ok(BaseSpecElm::Base(BaseType::Atom(atom_name.clone()))),
            _ => Err(format!("Expected atom literal but got {:?}", spec)),
        },
        x => Err(format!("Unexpected type format: {:?}", x)),
    }
}
