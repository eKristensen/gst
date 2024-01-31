use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, value},
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use super::{
    ast::{Clause, Expr, Exprs, FunCall, FunKind, MapPair, MapPairType, Var},
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::{fname, lit},
    pat::pats,
    terminals::{atom, var},
    top::{fun, fun_def},
};

// TODO: Common pattern for nested list, avoid manual rewrite!
fn expr_nested_list<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, head) = ws(exprs)(i)?;
    let head = vec![head];

    let (i, _) = ws(tag("|"))(i)?;
    let (i, tail) = ws(exprs)(i)?;
    let tail = vec![tail];
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::cerl_parser::ast::Expr::Cons(cons)))
}

fn case_of<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("case"))(i)?;
    let (i, exprs) = exprs(i)?;
    let (i, _) = ws(tag("of"))(i)?;
    let (i, clauses) = many0(clause)(i)?;
    let (i, _) = ws(tag("end"))(i)?;
    Ok((i, crate::cerl_parser::ast::Expr::Case(exprs, clauses)))
}

fn letrec<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("letrec"))(i)?;
    let (i, fundefs) = many0(ws(fun_def))(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, expressions) = exprs(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::LetRec(fundefs, expressions),
    ))
}

fn apply<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("apply"))(i)?;
    let (i, name) = ws(atom)(i)?;
    let (i, exprs_args) = comma_sep_list("(", ")", ws(exprs))(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::Call(
            FunCall {
                kind: FunKind::Apply,
                name,
            },
            exprs_args,
        ),
    ))
}

fn call<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("call"))(i)?;
    let (i, module) = ws(atom)(i)?;
    let (i, _) = ws(tag(":"))(i)?;
    let (i, name) = ws(atom)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::Call(
            FunCall {
                kind: FunKind::Call(module),
                name,
            },
            args,
        ),
    ))
}

fn primop<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("primop"))(i)?;
    let (i, name) = ws(atom)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::Call(
            FunCall {
                kind: FunKind::PrimOp,
                name,
            },
            args,
        ),
    ))
}

fn receive<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("receive"))(i)?;
    let (i, clauses) = many0(clause)(i)?;
    let (i, _) = ws(tag("after"))(i)?;
    let (i, timeout) = ws(exprs)(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, action) = ws(exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::Receive(clauses, timeout, action),
    ))
}

fn try_expr<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("try"))(i)?;
    let (i, arg) = ws(exprs)(i)?;
    let (i, _) = ws(tag("of"))(i)?;
    let (i, vars_) = comma_sep_list("<", ">", var)(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, body) = ws(exprs)(i)?;
    let (i, _) = ws(tag("catch"))(i)?;
    let (i, evars) = comma_sep_list("<", ">", var)(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, handler) = ws(exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Expr::Try(arg, vars_, body, evars, handler),
    ))
}

// TODO: Annotations can be any odd place, not implemented yet for maps
fn map_expr<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    map(
        delimited(
            ws(tag("~{")),
            // Note: It is impossible to flatten or evaluate keys or values of maps since expressions
            // might need to be evaluated to their true value which might depend on the current context.
            tuple((
                separated_list0(tag(","), map_pair),
                opt(map(
                    preceded(ws(tag("|")), alt((map(var, Expr::Var), map_expr))),
                    Box::new,
                )),
            )),
            ws(tag("}~")),
        ),
        |(o1, o2)| Expr::Map(o1, o2),
    )(i)
}

fn map_pair<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, MapPair, E> {
    map(
        tuple((
            expr,
            alt((
                value(MapPairType::Assoc, ws(tag("=>"))),
                value(MapPairType::Exact, ws(tag(":="))),
            )),
            expr,
        )),
        |(key, pair_type, value)| MapPair {
            pair_type,
            key,
            value,
        },
    )(i)
}

fn do_expr<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("do"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    let (i, exprs2) = ws(exprs)(i)?;
    Ok((i, crate::cerl_parser::ast::Expr::Do(exprs1, exprs2)))
}

fn catch<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("catch"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    Ok((i, crate::cerl_parser::ast::Expr::Catch(exprs1)))
}

// TODO: More elegant way to add opt_annotation.
fn clause<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Clause, E> {
    opt_annotation(clause_inner)(i)
}

fn clause_inner<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Clause, E> {
    let (i, pats) = pats(i)?;
    let (i, _) = ws(tag("when"))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::Clause {
            pats,
            when: exprs1,
            res: exprs2,
        },
    ))
}

fn let_in<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    let (i, _) = ws(tag("let"))(i)?;
    // Delay thinking about how the vars and exprs match in the equal sign
    let (i, vars) = vars(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((i, crate::cerl_parser::ast::Expr::Let(vars, exprs1, exprs2)))
}

fn vars<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, Vec<Var>, E> {
    opt_annotation(vars_inner)(i)
}

// TODO: Move? It is here in lack of a better fitting module
fn vars_inner<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&str, Vec<Var>, E> {
    alt((map(var, |o| vec![o]), comma_sep_list("<", ">", var)))(i)
}

fn expr<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    opt_annotation(expr_inner)(i)
}

fn expr_inner<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Expr, E> {
    ws(alt((
        map(var, crate::cerl_parser::ast::Expr::Var),
        map(fname, crate::cerl_parser::ast::Expr::Fname),
        map(lit, crate::cerl_parser::ast::Expr::Lit),
        map(fun, |fun| crate::cerl_parser::ast::Expr::Fun(fun)),
        expr_nested_list,
        map(
            comma_sep_list("[", "]", exprs),
            crate::cerl_parser::ast::Expr::Cons,
        ),
        map(
            comma_sep_list("{", "}", exprs),
            crate::cerl_parser::ast::Expr::Tuple,
        ),
        let_in,
        case_of,
        letrec,
        apply,
        call,
        primop,
        receive,
        try_expr,
        do_expr,
        catch,
        map_expr,
    )))(i)
}

pub fn exprs<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Exprs, E> {
    opt_annotation(exprs_inner)(i)
}

// TODO: Redundant opt_annotation here?
fn exprs_inner<
    'a,
    E: ParseError<&'a str> + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
>(
    i: &'a str,
) -> IResult<&str, Exprs, E> {
    ws(alt((
        map(expr, |o| crate::cerl_parser::ast::Exprs(vec![o])),
        map(
            comma_sep_list("<", ">", opt_annotation(expr)),
            crate::cerl_parser::ast::Exprs,
        ),
    )))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test case where the issue was the args list. Wrapping for completeness
    // TODO: Check output maybe?
    #[test]
    fn test_exprs_call_args_list() {
        assert!(exprs::<()>(
            " case call 'a':'b' (_0, 'new', 'neg') of <_2> when 'true' -> [] end "
        )
        .is_ok());
    }
    /*
    TODO: Test this

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    'init'/1 =
        fun (_0) ->
          {'ok',{'plus_state',_0,'undefined',~{}~}}
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    */
}
