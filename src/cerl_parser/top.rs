use nom::{combinator::map, multi::many0, sequence::tuple, IResult};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Attribute, FunDef, FunName, Module},
    expr::exprs,
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::{fname, lit},
    terminals::{atom, var},
};

pub fn fun_def(i: &str) -> IResult<&str, (FunName, FunDef), ErrorTree<&str>> {
    let (i, head) = fname(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, def) = opt_annotation(fun)(i)?;

    Ok((i, (head, def)))
}

pub fn fun(i: &str) -> IResult<&str, FunDef, ErrorTree<&str>> {
    let (i, _) = ws(tag("fun"))(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, args) = comma_sep_list("(", ")", var)(i)?;
    let (i, _) = ws(tag("->"))(i)?;

    let (i, exprs) = ws(exprs)(i)?;
    Ok((i, FunDef { args, body: exprs }))
}

// TODO: Attributes that are not in a list is parsed as if they are, e.g.
// 'mspec' = ['?neg !ready. ?int !int']]
// Results in
// Attribute { name: Atom("mspec"), value: Cons([Lit(Atom(Atom("?neg !ready. ?int !int")))]) }
// But it should be (please note the absence of Cons)
// Attribute { name: Atom("mspec"), value: Lit(Atom(Atom("?neg !ready. ?int !int"))) }
fn attribute(i: &str) -> IResult<&str, Attribute, ErrorTree<&str>> {
    ws(map(tuple((atom, ws(tag("=")), lit)), |(atom, _, val)| {
        Attribute {
            name: atom,
            value: val,
        }
    }))(i)
}

pub fn module(i: &str) -> IResult<&str, Module, ErrorTree<&str>> {
    opt_annotation(module_inner)(i)
}

// Top level module definition
fn module_inner(i: &str) -> IResult<&str, Module, ErrorTree<&str>> {
    // TODO: Check that "tag" requires "module" and does not work with partial data such as "mod"
    // Scan module
    let (i, _) = ws(tag("module"))(i)?;

    // Get module name.
    let (i, name) = ws(atom)(i)?;

    // Get exports
    let (i, exports) = ws(comma_sep_list("[", "]", fname))(i)?;

    // Require attributes keyword
    let (i, _) = ws(tag("attributes"))(i)?;

    // Get attributes
    let (i, attributes) = ws(comma_sep_list("[", "]", attribute))(i)?;

    // Module Body - Function definitions
    let (i, body) = many0(ws(fun_def))(i)?;

    // Require end keyword
    let (i, _) = ws(tag("end"))(i)?;

    Ok((
        i,
        Module {
            name,
            exports,
            attributes,
            body,
        },
    ))
}
