use nom::{bytes::complete::tag, combinator::map, multi::many0, sequence::tuple, IResult};

use super::{
    ast::{Attribute, Exprs, FunDef, Module, Var},
    expr::exprs,
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::{const_, fname},
    terminals::{atom, var},
};

pub fn fun(i: &str) -> IResult<&str, FunDef> {
    let (i, head) = fname(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, (args, exprs)) = opt_annotation(fun_inner)(i)?;

    Ok((
        i,
        FunDef {
            head,
            args,
            body: exprs,
        },
    ))
}

fn fun_inner(i: &str) -> IResult<&str, (Vec<Var>, Exprs)> {
    let (i, _) = ws(tag("fun"))(i)?;

    // TODO: function arguments parsing, must be able to parse variables
    let (i, args) = comma_sep_list("(", ")", var)(i)?;
    let (i, _) = ws(tag("->"))(i)?;

    let (i, exprs) = ws(exprs)(i)?;
    Ok((i, (args, exprs)))
}

fn attribute(i: &str) -> IResult<&str, Attribute> {
    ws(map(
        tuple((atom, ws(tag("=")), const_)),
        |(atom, _, val)| Attribute {
            name: atom,
            value: val,
        },
    ))(i)
}

pub fn module(i: &str) -> IResult<&str, Module> {
    opt_annotation(module_inner)(i)
}

// Top level module definition
fn module_inner(i: &str) -> IResult<&str, Module> {
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
    let (i, body) = many0(ws(fun))(i)?;

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
