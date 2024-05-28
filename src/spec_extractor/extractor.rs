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

pub fn base_spec_extractor(ast: &cerl_parser::ast::Module) -> BaseSpecDef {
    let mut base_spec_def: BaseSpecDef = BaseSpecDef(HashMap::new());

    // Use attributes to get spec
    for attribute in &ast.attributes {
        let Atom(a_name) = &attribute.name;
        if attribute.name == Atom("spec".to_owned()) {
            let new_base_spec = add_base_spec(&attribute.value);
            if new_base_spec.is_ok() {
                let (fun_name, base_specs) = new_base_spec.unwrap();
                if base_spec_def.0.contains_key(&fun_name) {
                    panic!(
                        "FATAL: Duplicate -session for function {}. Exiting now...",
                        fun_name
                    )
                } else {
                    base_spec_def.0.insert(fun_name, base_specs);
                }
            } else {
                println!(
                    "Warning: Failed base spec extraction. Reason: {:?}",
                    new_base_spec.err()
                )
            }
        }
        // TODO: Add support for custom type declarations?
    }
    base_spec_def
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
            return Err(format!("add_base_spec: -spec parse failure #1"));
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
        return Err(format!("get_fname_content: -spec parse failure #1"));
    };
    if spec.len() != 1 {
        return Err(format!("get_fname_content: -spec parse failure #2"));
    }
    let spec = spec.first().unwrap();
    let Lit::Tuple(spec) = spec else {
        return Err(format!("get_fname_content: -spec parse failure #3"));
    };

    // The tuple has two elements: Function name encoded as a two-element tuple and the inner content.
    // The function name is parsed here and the rest is returned
    if spec.len() != 2 {
        return Err(format!("get_fname_content: -spec parse failure #4"));
    }

    // Parse function name:
    let Lit::Tuple(fname_tuple) = spec.first().unwrap() else {
        return Err(format!("get_fname_content: -spec parse failure #5"));
    };
    if fname_tuple.len() != 2 {
        return Err(format!("get_fname_content: -spec parse failure #6"));
    }
    let Lit::Atom(fun_name) = fname_tuple.first().unwrap() else {
        return Err(format!("get_fname_content: -spec parse failure #6"));
    };
    let Lit::Int(arity) = &fname_tuple[1] else {
        return Err(format!("get_fname_content: -spec parse failure #7"));
    };

    // Sanity check
    if *arity < 0 {
        return Err(format!("get_fname_content: -spec parse failure #8"));
    }

    // Unsigned arity
    let arity = u64::try_from(*arity);
    if arity.is_err() {
        return Err(format!("get_fname_content: -spec parse failure #9"));
    }
    let arity = arity.unwrap();

    Ok((
        FunName {
            name: (*fun_name).clone(),
            arity,
        },
        &spec[1],
    ))
}

fn get_absform_clauses(spec: &Lit) -> Result<&Vec<Lit>, String> {
    // Expect top-level list and split each element. Very simple
    let Lit::Cons(spec) = spec else {
        return Err(format!("get_absform_clauses: -spec parse failure #1"));
    };
    Ok(spec)
}

fn get_absform_clause(spec: &Lit) -> Result<(Lit, Lit), String> {
    // Each clause starts with a certain type fun tag we need to check exists, and otherwise ignore
    // We check, but ignore the location info that is encoded in the the absform.
    let spec = check_tag_return_residual(spec, Atom("type".to_owned()))?;

    // First element after tag and location must be an atom that says 'fun'
    if Lit::Atom(Atom("fun".to_owned())) != spec[0] {
        return Err(format!("get_absform_clause: -spec parse failure #01"));
    }

    // First element after tag and location is a two-element list with the input and output.
    let Lit::Cons(io) = spec[1].clone() else {
        return Err(format!("get_absform_clause: -spec parse failure #09"));
    };
    if io.len() != 2 {
        return Err(format!("get_absform_clause: -spec parse failure #10"));
    }

    // We output input and output of the clause ready to be parsed later
    Ok((io[0].clone(), io[1].clone()))
}

fn get_absform_direct_type(spec: &Lit) -> Result<BaseSpecElm, String> {
    // A direct type, i.e. a type that is not a product type. General parsing similar to absform clause
    let spec = check_tag_return_residual(spec, Atom("type".to_owned()))?;

    // First element after tag and location is the type as an atom
    let Lit::Atom(atom_type_string) = spec[0].clone() else {
        return Err(format!("get_absform_direct_type: -spec parse failure #01"));
    };

    // No arguments to types are allowed (yet). Therefor next element must be empty list
    let Lit::Cons(args) = spec[1].clone() else {
        return Err(format!("get_absform_direct_type: -spec parse failure #02"));
    };
    if args.len() != 0 {
        return Err(format!("get_absform_direct_type: -spec parse failure #03"));
    }

    let base_type = atom_to_type(&atom_type_string)?;
    Ok(base_type)
}

fn get_absform_product_type(spec: &Lit) -> Result<Vec<BaseSpecElm>, String> {
    // Product type is a special kind of type that can nest other types.
    let spec = check_tag_return_residual(spec, Atom("type".to_owned()))?;

    // First element after tag and location is 'product'
    if Lit::Atom(Atom("product".to_owned())) != spec[0] {
        return Err(format!("get_absform_product_type: -spec parse failure #01"));
    }

    // Second element after tag and location is the composite type.
    let Lit::Cons(types_list) = spec[1].clone() else {
        return Err(format!("get_absform_product_type: -spec parse failure #02"));
    };

    // Each of these types is atoms that should be converted to base_types:
    let mut base_types_out: Vec<BaseSpecElm> = Vec::new();
    for elm in types_list {
        let Lit::Atom(atom_type_string) = elm else {
            return Err(format!("get_absform_product_type: -spec parse failure #03"));
        };
        let base_type = atom_to_type(&atom_type_string)?;
        base_types_out.push(base_type)
    }

    Ok(base_types_out)
}

fn get_absform_type(spec: &Lit) -> Result<Vec<BaseSpecElm>, String> {
    // Helper to try direct and product easily
    let specs = get_absform_product_type(spec);

    if (specs.is_ok()) {
        return Ok(specs.unwrap());
    }

    // Try single type instead
    let single_type = get_absform_direct_type(spec);

    if (single_type.is_ok()) {
        return Ok(vec![single_type.unwrap()]);
    }

    Err(format!(
        "No match found. Product type fail with {:?} and single type fail with {:?}",
        specs.err(),
        single_type.err()
    ))
}

fn atom_to_type(spec: &Atom) -> Result<BaseSpecElm, String> {
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
        _ => Ok(BaseSpecElm::Base(BaseType::Atom(spec.clone()))), // fallback
    }
}

fn check_tag_return_residual(spec: &Lit, tag: Atom) -> Result<&[Lit], String> {
    // Checks a tag and returns the content of the tag if a match is found.
    // We assume length is always four for now.
    let Lit::Tuple(spec) = spec else {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #01"
        ));
    };
    // The type function spec tuple is always four element long
    if spec.len() != 4 {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #02"
        ));
    }

    // Sanity checks
    // First element must be an atom that says 'type'
    if Lit::Atom(tag) != spec[0] {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #03"
        ));
    }

    // Second element is a tuple with two integers
    let Lit::Tuple(location) = spec[1].clone() else {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #04"
        ));
    };
    if location.len() != 2 {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #05"
        ));
    }
    let Lit::Int(_l1) = location[0] else {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #06"
        ));
    };
    let Lit::Int(_l2) = location[1] else {
        return Err(format!(
            "check_tag_return_residual: -spec parse failure #07"
        ));
    };

    Ok(&spec[2..])
}
