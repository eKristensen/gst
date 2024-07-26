use nom::{
    combinator::map,
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Anno, AnnoAtom, AnnoFun, AnnoFunName, AnnoLit, Attribute, FunDef, Module},
    expressions::{anno_function_name, fun_expr, literal},
    helpers::{comma_sep_list, opt_annotation, ws},
    tokeniser::atom,
};

pub fn module_definition(i: &str) -> IResult<&str, Module, ErrorTree<&str>> {
    map(opt_annotation(module_definition_inner), |(inner, anno)| {
        inner.anno = anno
    })(i)
}

fn module_definition_inner(i: &str) -> IResult<&str, Module, ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("module")),
            tuple((
                ws(atom),
                ws(module_export),
                ws(module_attribute),
                ws(module_defs),
            )),
            ws(tag("end")),
        ),
        |(name, exports, attributes, defs)| Module {
            anno: Anno::Unknown,
            name,
            exports,
            attributes,
            defs,
        },
    )(i)
}

fn module_export(i: &str) -> IResult<&str, Vec<AnnoFunName>, ErrorTree<&str>> {
    ws(comma_sep_list("[", "]", anno_function_name))(i)
}

fn module_attribute(i: &str) -> IResult<&str, Vec<Attribute>, ErrorTree<&str>> {
    preceded(
        ws(tag("attributes")),
        ws(comma_sep_list("[", "]", attribute)),
    )(i)
}

fn anno_atom(i: &str) -> IResult<&str, AnnoAtom, ErrorTree<&str>> {
    map(opt_annotation(atom), |(name, anno)| AnnoAtom { anno, name })(i)
}

fn anno_literal(i: &str) -> IResult<&str, AnnoLit, ErrorTree<&str>> {
    map(opt_annotation(literal), |(inner, anno)| AnnoLit {
        anno,
        inner,
    })(i)
}

fn module_defs(i: &str) -> IResult<&str, Vec<FunDef>, ErrorTree<&str>> {
    // TODO: For debuggin the number of functions required is set to one.
    // TODO: Change to many0 instead of many1
    many1(ws(function_definition))(i)
}

fn function_definition(i: &str) -> IResult<&str, FunDef, ErrorTree<&str>> {
    map(
        tuple((ws(anno_function_name), ws(tag("=")), ws(anno_fun))),
        |(name, _, body)| FunDef { name, body },
    )(i)
}

fn anno_fun(i: &str) -> IResult<&str, AnnoFun, ErrorTree<&str>> {
    map(opt_annotation(fun_expr), |(fun, anno)| AnnoFun {
        anno,
        fun,
    })(i)
}

fn attribute(i: &str) -> IResult<&str, Attribute, ErrorTree<&str>> {
    ws(map(
        tuple((anno_atom, ws(tag("=")), anno_literal)),
        |(name, _, value)| Attribute { name, value },
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::attribute;

    #[test]
    fn test_attribute_with_list() {
        assert_eq!(attribute("'file' =\n\t\t    %% Line 1\n\t\t    [{[116|[101|[115|[116|[115|[47|[99|[101|[114|[108|[95|[112|[97|[114|[115|[101|[114|[47|[112|[97|[115|[115|[47|[115|[105|[109|[112|[108|[101|[45|[48|[48|[46|[101|[114|[108]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]],1}]\n").is_ok(),
            true
        );
    }
}
