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
        let Atom(a_name) = &attribute.name;
        if *a_name == "spec" {
            let new_base_spec = add_base_spec(&attribute.value);
            if new_base_spec.is_ok() {
                let (fun_name, base_specs) = new_base_spec.unwrap();
                if base_spec_def.0.contains_key(&fun_name) {
                    return Err(format!(
                        "FATAL: Duplicate -session for function {}. Exiting now...",
                        &fun_name
                    ));
                } else {
                    base_spec_def.0.insert(fun_name.clone(), base_specs);
                }
            } else {
                return Err(format!(
                    "Warning: Failed base spec extraction. Reason: {:?}",
                    new_base_spec.err()
                ));
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
        let input = get_absform_type(&input)?;
        let BaseSpecElm::Base(output) = get_absform_direct_type(&output)? else {
            return Err("add_base_spec: -spec parse failure #1".to_string());
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
    let arity = u64::try_from(*arity);
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
    let spec = check_tag_return_residual(spec, &Atom("type".to_owned()))?;

    // First element after tag and location must be an atom that says 'fun'
    if Lit::Atom(Atom("fun".to_owned())) != spec[0] {
        return Err("get_absform_clause: -spec parse failure #01".to_string());
    }

    // First element after tag and location is a two-element list with the input and output.
    let Lit::Cons(io) = spec[1].clone() else {
        return Err("get_absform_clause: -spec parse failure #09".to_string());
    };
    if io.len() != 2 {
        return Err("get_absform_clause: -spec parse failure #10".to_string());
    }

    // We output input and output of the clause ready to be parsed later
    Ok((io[0].clone(), io[1].clone()))
}

fn get_absform_direct_type(spec: &Lit) -> Result<BaseSpecElm, String> {
    // A direct type, i.e. a type that is not a product type. General parsing similar to absform clause
    let spec = check_tag_return_residual(spec, &Atom("type".to_owned()))?;

    // First element after tag and location is the type as an atom
    let Lit::Atom(atom_type_string) = spec[0].clone() else {
        return Err("get_absform_direct_type: -spec parse failure #01".to_string());
    };

    // No arguments to types are allowed (yet). Therefor next element must be empty list
    let Lit::Cons(args) = spec[1].clone() else {
        return Err("get_absform_direct_type: -spec parse failure #02".to_string());
    };
    if !args.is_empty() {
        return Err("get_absform_direct_type: -spec parse failure #03".to_string());
    }

    let base_type = atom_to_base_type(&atom_type_string)?;
    Ok(base_type)
}

fn get_absform_user_type(spec: &Lit) -> Result<BaseSpecElm, String> {
    // A direct user type, i.e. a type that is not a product type. General parsing similar to absform clause
    let spec = check_tag_return_residual(spec, &Atom("user_type".to_owned()))?;

    // First element after tag and location is the type as an atom
    let Lit::Atom(atom_type_string) = spec[0].clone() else {
        return Err("get_absform_user_type: -spec parse failure #01".to_string());
    };

    // No arguments to types are allowed (yet). Therefor next element must be empty list
    let Lit::Cons(args) = spec[1].clone() else {
        return Err("get_absform_user_type: -spec parse failure #02".to_string());
    };
    if !args.is_empty() {
        return Err("get_absform_user_type: -spec parse failure #03".to_string());
    }

    let base_type = atom_to_session_type(&atom_type_string)?;
    Ok(base_type)
}

fn get_absform_product_type(spec: &Lit) -> Result<Vec<BaseSpecElm>, String> {
    // Product type is a special kind of type that can nest other types.
    let spec = check_tag_return_residual(spec, &Atom("type".to_owned()))?;

    // First element after tag and location is 'product'
    if Lit::Atom(Atom("product".to_owned())) != spec[0] {
        return Err("get_absform_product_type: -spec parse failure #01".to_string());
    }

    // Second element after tag and location is the composite type.
    let Lit::Cons(types_list) = spec[1].clone() else {
        return Err("get_absform_product_type: -spec parse failure #02".to_string());
    };

    // Each of these types is atoms that should be converted to base_types:
    let mut base_types_out: Vec<BaseSpecElm> = Vec::new();
    for elm in types_list {
        let base_type = get_absform_direct_type(&elm);
        if let Ok(base_type) = base_type {
            base_types_out.push(base_type);
            continue;
        }

        let user_type = get_absform_user_type(&elm);
        if let Ok(user_type) = user_type {
            base_types_out.push(user_type);
            continue;
        }

        return Err(format!(
            "get_absform_product_type: -spec parse failure #03, debug info: {:?}, {:?}",
            base_type.err(),
            user_type.err()
        ));
    }

    Ok(base_types_out)
}

fn get_absform_type(spec: &Lit) -> Result<Vec<BaseSpecElm>, String> {
    // Helper to try direct and product easily
    let specs = get_absform_product_type(spec);

    if let Ok(specs) = specs {
        return Ok(specs);
    }

    // Try single type instead
    let single_type = get_absform_direct_type(spec);

    if single_type.is_ok() {
        return Ok(vec![single_type.unwrap()]);
    }

    Err(format!(
        "No match found. Product type fail with {:?} and single type fail with {:?}",
        specs.err(),
        single_type.err()
    ))
}

fn atom_to_base_type(spec: &Atom) -> Result<BaseSpecElm, String> {
    // TODO: All unknown types are accepted as atom constants!
    let Atom(atom) = spec;
    match atom.as_str() {
        "new" => Ok(BaseSpecElm::New),
        "consume" => Ok(BaseSpecElm::Consume),
        "pid" => Ok(BaseSpecElm::Base(BaseType::Pid)),
        "reference" => Ok(BaseSpecElm::Base(BaseType::Reference)),
        "integer" => Ok(BaseSpecElm::Base(BaseType::Integer)),
        "float" => Ok(BaseSpecElm::Base(BaseType::Float)),
        "boolean" => Ok(BaseSpecElm::Base(BaseType::Boolean)),
        // TODO: Support cons/tuple types
        _ => Err("Unsupported type used".to_string()), // fallback
    }
}

fn atom_to_session_type(spec: &Atom) -> Result<BaseSpecElm, String> {
    // TODO: All unknown types are accepted as atom constants!
    let Atom(atom) = spec;
    match atom.as_str() {
        "new" => Ok(BaseSpecElm::New),
        "consume" => Ok(BaseSpecElm::Consume),
        _ => Err("No custom types supported yet.".to_string()), // fallback
    }
}

fn check_tag_return_residual<'a>(spec: &'a Lit, tag: &Atom) -> Result<&'a [Lit], String> {
    // Checks a tag and returns the content of the tag if a match is found.
    // We assume length is always four for now.
    let Lit::Tuple(spec) = spec else {
        return Err("check_tag_return_residual: -spec parse failure #01".to_string());
    };
    // The type function spec tuple is always four element long
    if spec.len() != 4 {
        return Err("check_tag_return_residual: -spec parse failure #02".to_string());
    }

    // Sanity checks
    // First element must be an atom that says 'type'
    // TODO: Can be performance optimized: Unwrap spec[0] and compare pointer values instead of clone.
    if Lit::Atom(tag.clone()) != spec[0] {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #03. DEBUG info: spec is: {:?}, expected tag is: {:?}", spec, tag
        ));
    }

    // Second element is a tuple with two integers
    let Lit::Tuple(location) = spec[1].clone() else {
        return Err("check_tag_return_residual: -spec parse failure #04".to_string());
    };
    if location.len() != 2 {
        return Err("check_tag_return_residual: -spec parse failure #05".to_string());
    }
    let Lit::Int(_l1) = location[0] else {
        return Err("check_tag_return_residual: -spec parse failure #06".to_string());
    };
    let Lit::Int(_l2) = location[1] else {
        return Err("check_tag_return_residual: -spec parse failure #07".to_string());
    };

    Ok(&spec[2..])
}
