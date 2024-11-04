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
    cinput::CInput,
    expressions::{anno_function_name, fun_expr, literal},
    helpers::{comma_sep_list, loc, opt_annotation, wsa, wsc},
    tokeniser::atom,
};

// Top/parser start location
// WSA OK
pub fn run_parser(i: CInput) -> IResult<CInput, AnnoModule, ErrorTree<CInput>> {
    // We need to remove whitespace if any before we can start the parser
    // Start the parser with the top-level compoment: optionally annotated module:
    preceded(wsc, anno_module_definition)(i)
}

// WSA OK
fn anno_module_definition(i: CInput) -> IResult<CInput, AnnoModule, ErrorTree<CInput>> {
    map(
        loc(opt_annotation(module_definition)),
        |(loc, (inner, anno))| AnnoModule { loc, anno, inner },
    )(i)
}

// WSA OK
fn module_definition(i: CInput) -> IResult<CInput, Module, ErrorTree<CInput>> {
    map(
        loc(delimited(
            wsa(tag("module")),
            tuple((atom, module_export, module_attribute, module_defs)),
            wsa(tag("end")),
        )),
        |(loc, (name, exports, attributes, defs))| Module {
            loc,
            name,
            exports,
            attributes,
            defs,
        },
    )(i)
}

// WSA OK
fn module_export(i: CInput) -> IResult<CInput, Vec<AnnoFunName>, ErrorTree<CInput>> {
    comma_sep_list("[", "]", anno_function_name)(i)
}

// WSA OK
fn module_attribute(i: CInput) -> IResult<CInput, Vec<Attribute>, ErrorTree<CInput>> {
    preceded(wsa(tag("attributes")), comma_sep_list("[", "]", attribute))(i)
}

// WSA OK
fn anno_atom(i: CInput) -> IResult<CInput, Rc<AnnoAtom>, ErrorTree<CInput>> {
    map(loc(opt_annotation(atom)), |(loc, (name, anno))| {
        AnnoAtom { loc, anno, name }.into()
    })(i)
}

// WSA OK
fn anno_literal(i: CInput) -> IResult<CInput, Rc<AnnoLit>, ErrorTree<CInput>> {
    map(loc(opt_annotation(literal)), |(loc, (inner, anno))| {
        AnnoLit {
            loc,
            anno,
            inner: inner.into(),
        }
        .into()
    })(i)
}

// WSA OK
fn module_defs(i: CInput) -> IResult<CInput, Vec<FunDef>, ErrorTree<CInput>> {
    function_definitions(i)
}

// WSA OK
pub fn function_definitions(i: CInput) -> IResult<CInput, Vec<FunDef>, ErrorTree<CInput>> {
    // TODO: For debuggin the number of functions required is set to one.
    // TODO: Change to many0 instead of many1
    many0(function_definition)(i)
}

// WSA OK
// Do not Rc as FunDef is used in Vec
fn function_definition(i: CInput) -> IResult<CInput, FunDef, ErrorTree<CInput>> {
    map(
        loc(tuple((anno_function_name, wsa(tag("=")), anno_fun))),
        |(loc, (name, _, body))| FunDef {
            loc,
            name: name.into(),
            body,
        },
    )(i)
}

// WSA OK
fn anno_fun(i: CInput) -> IResult<CInput, Rc<AnnoFun>, ErrorTree<CInput>> {
    map(loc(opt_annotation(fun_expr)), |(loc, (fun, anno))| {
        AnnoFun { loc, anno, fun }.into()
    })(i)
}

// WSA OK
fn attribute(i: CInput) -> IResult<CInput, Attribute, ErrorTree<CInput>> {
    map(
        loc(tuple((anno_atom, wsa(tag("=")), anno_literal))),
        |(loc, (name, _, value))| Attribute { loc, name, value },
    )(i)
}

#[cfg(test)]
mod tests {
    use super::attribute;
    use crate::cerl_parser::cinput::CInput;

    #[test]
    fn test_attribute_with_list() {
        assert!(attribute(CInput::new("'file' =\n\t\t    %% Line 1\n\t\t    [{[116|[101|[115|[116|[115|[47|[99|[101|[114|[108|[95|[112|[97|[114|[115|[101|[114|[47|[112|[97|[115|[115|[47|[115|[105|[109|[112|[108|[101|[45|[48|[48|[46|[101|[114|[108]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],1}]\n")).is_ok(),
        );
    }
}
