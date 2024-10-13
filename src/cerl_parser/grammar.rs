use std::rc::Rc;

use nom::{
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{AnnoAtom, AnnoFun, AnnoFunName, AnnoLit, AnnoModule, Attribute, FunDef, Module},
    expressions::{anno_function_name, fun_expr, literal},
    helpers::{comma_sep_list, opt_annotation, wsa, wsc},
    tokeniser::atom,
};

// Top/parser start location
// WSA OK
pub fn run_parser(i: &str) -> IResult<&str, AnnoModule, ErrorTree<&str>> {
    // We need to remove whitespace if any before we can start the parser
    let (i, _) = wsc(i)?;
    // Start the parser with the top-level compoment: optionally annotated module:
    anno_module_definition(i)
}

// WSA OK
fn anno_module_definition(i: &str) -> IResult<&str, AnnoModule, ErrorTree<&str>> {
    map(opt_annotation(module_definition), |(inner, anno)| {
        AnnoModule { anno, inner }
    })(i)
}

// WSA OK
fn module_definition(i: &str) -> IResult<&str, Module, ErrorTree<&str>> {
    map(
        delimited(
            wsa(tag("module")),
            tuple((atom, module_export, module_attribute, module_defs)),
            wsa(tag("end")),
        ),
        |(name, exports, attributes, defs)| Module {
            name,
            exports,
            attributes,
            defs,
        },
    )(i)
}

// WSA OK
fn module_export(i: &str) -> IResult<&str, Vec<AnnoFunName>, ErrorTree<&str>> {
    comma_sep_list("[", "]", anno_function_name)(i)
}

// WSA OK
fn module_attribute(i: &str) -> IResult<&str, Vec<Attribute>, ErrorTree<&str>> {
    preceded(wsa(tag("attributes")), comma_sep_list("[", "]", attribute))(i)
}

// WSA OK
fn anno_atom(i: &str) -> IResult<&str, Rc<AnnoAtom>, ErrorTree<&str>> {
    map(opt_annotation(atom), |(name, anno)| {
        AnnoAtom { anno, name }.into()
    })(i)
}

// WSA OK
fn anno_literal(i: &str) -> IResult<&str, Rc<AnnoLit>, ErrorTree<&str>> {
    map(opt_annotation(literal), |(inner, anno)| {
        AnnoLit {
            anno,
            inner: inner.into(),
        }
        .into()
    })(i)
}

// WSA OK
fn module_defs(i: &str) -> IResult<&str, Vec<FunDef>, ErrorTree<&str>> {
    function_definitions(i)
}

// WSA OK
pub fn function_definitions(i: &str) -> IResult<&str, Vec<FunDef>, ErrorTree<&str>> {
    // TODO: For debuggin the number of functions required is set to one.
    // TODO: Change to many0 instead of many1
    many0(function_definition)(i)
}

// WSA OK
// Do not Rc as FunDef is used in Vec
fn function_definition(i: &str) -> IResult<&str, FunDef, ErrorTree<&str>> {
    map(
        tuple((anno_function_name, wsa(tag("=")), anno_fun)),
        |(name, _, body)| FunDef {
            name: name.into(),
            body,
        },
    )(i)
}

// WSA OK
fn anno_fun(i: &str) -> IResult<&str, Rc<AnnoFun>, ErrorTree<&str>> {
    map(opt_annotation(fun_expr), |(fun, anno)| {
        AnnoFun { anno, fun }.into()
    })(i)
}

// WSA OK
fn attribute(i: &str) -> IResult<&str, Attribute, ErrorTree<&str>> {
    map(
        tuple((anno_atom, wsa(tag("=")), anno_literal)),
        |(name, _, value)| Attribute { name, value },
    )(i)
}

#[cfg(test)]
mod tests {
    use super::attribute;

    #[test]
    fn test_attribute_with_list() {
        assert!(attribute("'file' =\n\t\t    %% Line 1\n\t\t    [{[116|[101|[115|[116|[115|[47|[99|[101|[114|[108|[95|[112|[97|[114|[115|[101|[114|[47|[112|[97|[115|[115|[47|[115|[105|[109|[112|[108|[101|[45|[48|[48|[46|[101|[114|[108]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],1}]\n").is_ok(),
        );
    }
}
