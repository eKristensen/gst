// Purpose: take in Core Erlang AST and output usable base spec type specification

use std::collections::HashMap;

use crate::{
    cerl_parser::{
        self,
        ast::{Anno, Atom, FunNameInner, Lit, LitInner},
    },
    contract_cerl::types::BaseType,
    st_parser::spec_extractor::flatten_cons,
};

use super::ast::{BaseSpec, BaseSpecDef, BaseSpecElm, BaseSpecs};

pub fn base_spec_extractor(ast: &cerl_parser::ast::Module) -> Result<BaseSpecDef, String> {
    let mut base_spec_def: BaseSpecDef = BaseSpecDef(HashMap::new());

    // Use attributes to get spec
    for attribute in &ast.inner.attributes {
        let a_name = &attribute.name.name;
        if *a_name == "spec" {
            match add_base_spec(&attribute.value) {
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

fn add_base_spec(spec: &Lit) -> Result<(FunNameInner, BaseSpecs), String> {
    // Alright I have it with a simple spec
    // But how does it look when spec contains ; ???
    let (fname, spec) = get_fname_content(spec)?;
    let absform_clauses = get_absform_clauses(&spec)?;
    let mut base_specs: BaseSpecs = BaseSpecs(Vec::new());
    for absform_clause in absform_clauses {
        let (input, output) = get_absform_clause(&absform_clause)?;
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
fn get_fname_content(spec: &Lit) -> Result<(FunNameInner, Lit), String> {
    // Outer wrapper is a list with a two-element tuple
    /*let LitInner::Cons(_spec) = spec.inner else {
        return Err("get_fname_content expected outer cons".to_string());
    };*/

    // Flatten Cons list
    let spec = flatten_cons(&spec.inner);
    if spec.len() != 1 {
        return Err("get_fname_content expected single element".to_string());
    }

    let spec = spec.first().unwrap();

    let LitInner::Tuple(spec) = spec else {
        return Err("get_fname_content: Main tuple missing".to_string());
    };
    // The main spec tuple
    // The tuple has two elements: Function name encoded as a two-element tuple and the inner content.
    let &[fname_tuple, inner_spec] = &spec.as_slice() else {
        return Err("get_fname_content: Main tuple wrong format".to_string());
    };

    let LitInner::Tuple(fname_tuple) = fname_tuple.inner.clone() else {
        return Err("get_fname_content: Fun-name must be in a tuple".to_string());
    };
    let &[Lit {
        anno: _anno0,
        inner: LitInner::Atom(fun_name),
    }, Lit {
        anno: _anno1,
        inner: LitInner::Int(arity),
    }] = &fname_tuple.as_slice()
    else {
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
        FunNameInner {
            name: Atom {
                anno: Anno(None),
                name: (*fun_name).clone(),
            },
            arity,
        },
        inner_spec.clone(),
    ))
}

fn get_absform_clauses(spec: &Lit) -> Result<Vec<LitInner>, String> {
    // Expect top-level list and split each element. Very simple
    let LitInner::Cons(_spec) = spec.inner.clone() else {
        return Err("get_absform_clauses: -spec parse failure #1".to_string());
    };

    // Flatten Cons list
    let spec = flatten_cons(&spec.inner);

    Ok(spec)
}

fn get_absform_clause(spec: &LitInner) -> Result<(LitInner, LitInner), String> {
    // Each clause starts with a certain type fun tag we need to check exists, and otherwise ignore
    // We check, but ignore the location info that is encoded in the the absform.
    let LitInner::Tuple(spec) = spec else {
        return Err("get_absform_clause must be in a tuple".to_string());
    };

    let &[Lit {
        anno: _anno0,
        inner: LitInner::Atom(tag),
    }, _, Lit {
        anno: _anno1,
        inner: LitInner::Atom(tag2),
    }, Lit {
        anno: _anno2,
        inner: io,
    }] = &spec.as_slice()
    else {
        return Err("get_absform_types: Wrong length tuple.".to_string());
    };
    if *tag != "type".to_owned() || *tag2 != "fun".to_owned() {
        return Err("get_absform_clause wrong tags in tuple".to_string());
    }

    // Flatten Cons list
    let LitInner::Cons(_cons) = io else {
        return Err("var io must be cons here. it is not".to_string());
    };
    let io = flatten_cons(io);

    if io.len() != 2 {
        return Err("get_absform_clause: -spec parse failure #10".to_string());
    }

    // We output input and output of the clause ready to be parsed later
    Ok((io[0].clone(), io[1].clone()))
}

fn get_absform_types(spec_in: &LitInner) -> Result<Vec<BaseSpecElm>, String> {
    // Product type is a special kind of type that can nest other types.
    let LitInner::Tuple(spec) = spec_in else {
        return Err("get_absform_product_type can only be in a tuple.".to_string());
    };

    // TODO: Not ignore Anno/check content of annotation
    let &[Lit {
        anno: _anno0,
        inner: LitInner::Atom(tag),
    }, _, Lit {
        anno: _anno1,
        inner: LitInner::Atom(tag2),
    }, Lit {
        anno: _anno_2,
        inner: types_list,
    }] = &spec.as_slice()
    else {
        return Err("get_absform_types: Wrong length tuple.".to_string());
    };
    if *tag != "type".to_owned() || *tag2 != "product".to_owned() {
        // Not product type assume a it is a ordinary type instead
        let single_type = get_absform_type(spec_in)?;
        return Ok(vec![single_type]);
    }

    // Flatten Cons list
    let LitInner::Cons(_types_list) = types_list else {
        return Err("types_list must be a list".to_string());
    };
    let types_list = flatten_cons(types_list);

    // Each of these types should be parsed:
    let mut base_types_out: Vec<BaseSpecElm> = Vec::new();
    for elm in types_list {
        let elm_type = get_absform_type(&elm)?;
        base_types_out.push(elm_type);
    }

    Ok(base_types_out)
}

fn get_absform_type(spec: &LitInner) -> Result<BaseSpecElm, String> {
    // Expecting a tuple
    let LitInner::Tuple(spec) = spec else {
        return Err("get_absform_type expected tuple".to_string());
    };

    // Type matching
    // TODO: Not ignore Anno
    match &spec.as_slice() {
        &[Lit {
            anno: _anno0,
            inner: LitInner::Atom(tag),
        }, _, Lit {
            anno: _anno1,
            inner: LitInner::Atom(tag2),
        }, Lit {
            anno: _anno2,
            inner: args,
        }] => match (tag.as_str(), tag2.as_str(), flatten_cons(args).as_slice()) {
            ("user_type", "new", &[]) => Ok(BaseSpecElm::New),
            ("user_type", "consume", &[]) => Ok(BaseSpecElm::Consume),
            ("type", "pid", &[]) => Ok(BaseSpecElm::Base(BaseType::Pid)),
            ("type", "reference", &[]) => Ok(BaseSpecElm::Base(BaseType::Reference)),
            ("type", "integer", &[]) => Ok(BaseSpecElm::Base(BaseType::Integer)),
            ("type", "float", &[]) => Ok(BaseSpecElm::Base(BaseType::Float)),
            ("type", "boolean", &[]) => Ok(BaseSpecElm::Base(BaseType::Boolean)),
            ("type", "list", &[]) => Ok(BaseSpecElm::Base(BaseType::List)),
            ("type", "string", &[]) => Ok(BaseSpecElm::Base(BaseType::String)),
            ("type", type_name, type_args) => Err(format!(
                "Unknown type {:?} with args {:?}",
                type_name, type_args
            )),
            ("user_type", type_name, type_args) => Err(format!(
                "Unknown user_type {:?} with args {:?}",
                type_name, type_args
            )),
            _ => Err("Could not match any type".to_string()),
        },
        &[Lit {
            anno: _anno0,
            inner: LitInner::Atom(tag),
        }, _, Lit {
            anno: _anno1,
            inner: LitInner::Atom(tag2),
        }, Lit {
            anno: _anno2,
            inner: LitInner::Atom(tag3),
        }] => match (tag.as_str(), tag2.as_str(), tag3.as_str()) {
            ("type", "map", "any") => Ok(BaseSpecElm::Base(BaseType::Map)),
            ("type", "tuple", "any") => {
                Ok(BaseSpecElm::Base(BaseType::Tuple(vec![BaseType::Term])))
            }
            x => Err(format!(
                "Unknown type where map or tuple was expected: {:?}",
                x
            )),
        },
        &[Lit {
            anno: _anno0,
            inner: LitInner::Atom(tag),
        }, _, Lit {
            anno: _anno1,
            inner: LitInner::Atom(tag2),
        }] => match (tag.as_str(), tag2) {
            ("atom", atom_name) => Ok(BaseSpecElm::Base(BaseType::Atom(atom_name.clone()))),
            _ => Err("Expected atom literal".to_string()),
        },
        x => Err(format!("Unexpected type format: {:?}", x)),
    }
}
