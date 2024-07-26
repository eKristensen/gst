use nom::{
    branch::alt,
    combinator::{map, opt, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{Anno, AnnoFunName, Clause, Expr, FunCall, FunExpr, Lit, MapPair, MapPairType, Var},
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::{fname, lit},
    terminals::{atom, var},
};

pub fn anno_function_name(i: &str) -> IResult<&str, AnnoFunName, ErrorTree<&str>> {}

pub fn fun_expr(i: &str) -> IResult<&str, FunExpr, ErrorTree<&str>> {}

pub fn literal(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {}

pub fn atomic_literal(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {}

// TODO: Common pattern for nested list, avoid manual rewrite!
fn expr_nested_list(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, head) = ws(exprs)(i)?;
    let head = [head];

    let (i, _) = ws(tag("|"))(i)?;
    let (i, tail) = ws(exprs)(i)?;
    let tail = [tail];
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::cerl_parser::ast::ExprInner::Cons(cons)))
}

fn case_of(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("case"))(i)?;
    let (i, exprs) = exprs(i)?;
    let (i, _) = ws(tag("of"))(i)?;
    let (i, clauses) = many1(clause)(i)?;
    let (i, _) = ws(tag("end"))(i)?;
    Ok((i, crate::cerl_parser::ast::ExprInner::Case(exprs, clauses)))
}

fn letrec(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("letrec"))(i)?;
    let (i, fundefs) = many1(ws(top_fun_def))(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, expressions) = exprs(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::LetRec(fundefs, expressions),
    ))
}

fn apply(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("apply"))(i)?;
    let (i, exprs0) = ws(exprs)(i)?; // Note: Apparently fname is used here
    let (i, exprs_args) = comma_sep_list("(", ")", ws(exprs))(i)?;
    // TODO: Find a way to reintroduce sanity check. Maybe well-formed later?
    /*
    let FunName { name, arity } = fname.clone();
    if arity != exprs_args.len() {
        panic!(
            "Sanity check for {:?} failed. Expected {} args but found {}",
            name,
            arity,
            exprs_args.len()
        )
    };
    */
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::Call(FunCall::Apply(exprs0), exprs_args),
    ))
}

fn call(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("call"))(i)?;
    let (i, module) = ws(exprs)(i)?;
    let (i, _) = ws(tag(":"))(i)?;
    let (i, call_name) = ws(exprs)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::Call(FunCall::Call(module, call_name), args),
    ))
}

fn primop(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("primop"))(i)?;
    let (i, name) = ws(atom)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::Call(FunCall::PrimOp(name), args),
    ))
}

fn receive(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("receive"))(i)?;
    let (i, clauses) = many0(clause)(i)?;
    let (i, _) = ws(tag("after"))(i)?;
    let (i, timeout) = ws(exprs)(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, action) = ws(exprs)(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::Receive(clauses, timeout, action),
    ))
}

fn try_expr(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
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
        crate::cerl_parser::ast::ExprInner::Try(arg, vars_, body, evars, handler),
    ))
}

// TODO: Annotations can be any odd place, not implemented yet for maps
// TODO: This parse function is hard to read.
fn map_expr(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("~{")),
            // Note: It is impossible to flatten or evaluate keys or values of maps since expressions
            // might need to be evaluated to their true value which might depend on the current context.
            tuple((
                separated_list0(tag(","), map_pair),
                opt(map(
                    preceded(
                        ws(tag("|")),
                        alt((
                            map(var, |o| Expr {
                                // TODO: Maybe converting var to expr is not a good
                                // idea?
                                anno: Anno(None),
                                inner: ExprInner::Var(o),
                            }),
                            map(opt_annotation(map_expr), |(m_expr, anno)| Expr {
                                anno,
                                inner: m_expr,
                            }),
                        )),
                    ),
                    Box::new,
                )),
            )),
            ws(tag("}~")),
        ),
        |(pairs, m_expr)| ExprInner::Map(pairs, m_expr),
    )(i)
}

fn map_pair(i: &str) -> IResult<&str, MapPair, ErrorTree<&str>> {
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

fn do_expr(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("do"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    let (i, exprs2) = ws(exprs)(i)?;
    Ok((i, crate::cerl_parser::ast::ExprInner::Do(exprs1, exprs2)))
}

fn catch(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("catch"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    Ok((i, crate::cerl_parser::ast::ExprInner::Catch(exprs1)))
}

// TODO: More elegant way to add opt_annotation.
fn clause(i: &str) -> IResult<&str, Clause, ErrorTree<&str>> {
    map(opt_annotation(clause_inner), |(inner, anno)| Clause {
        anno,
        inner,
    })(i)
}

fn clause_inner(i: &str) -> IResult<&str, ClauseInner, ErrorTree<&str>> {
    let (i, pats) = pats(i)?;
    let (i, _) = ws(tag("when"))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ClauseInner {
            pats,
            when: exprs1,
            res: exprs2,
        },
    ))
}

fn let_in(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    let (i, _) = ws(tag("let"))(i)?;
    // Delay thinking about how the vars and exprs match in the equal sign
    let (i, vars) = vars(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((
        i,
        crate::cerl_parser::ast::ExprInner::Let(vars, exprs1, exprs2),
    ))
}

fn vars(i: &str) -> IResult<&str, Vec<Var>, ErrorTree<&str>> {
    alt((map(var, |o| vec![o]), comma_sep_list("<", ">", var)))(i)
}

fn expr(i: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    map(opt_annotation(expr_inner), |(inner, anno)| Expr {
        anno,
        inner,
    })(i)
}

fn expr_inner(i: &str) -> IResult<&str, ExprInner, ErrorTree<&str>> {
    ws(alt((
        map(var, crate::cerl_parser::ast::ExprInner::Var),
        map(fname, crate::cerl_parser::ast::ExprInner::Fname),
        map(lit, crate::cerl_parser::ast::ExprInner::Lit),
        map(fun_def, crate::cerl_parser::ast::ExprInner::Fun),
        expr_nested_list,
        map(
            comma_sep_list("[", "]", exprs),
            crate::cerl_parser::ast::ExprInner::Cons,
        ),
        map(
            comma_sep_list("{", "}", exprs),
            crate::cerl_parser::ast::ExprInner::Tuple,
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

pub fn exprs(i: &str) -> IResult<&str, Exprs, ErrorTree<&str>> {
    ws(alt((
        map(expr, |o| crate::cerl_parser::ast::Exprs::One(Box::new(o))),
        map(
            comma_sep_list("<", ">", expr),
            crate::cerl_parser::ast::Exprs::Many,
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
        assert!(
            exprs(" case call 'a':'b' (_0, 'new', 'neg') of <_2> when 'true' -> [] end ").is_ok()
        );
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
