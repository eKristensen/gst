use nom::{branch::alt, bytes::complete::tag, combinator::map, multi::many0, IResult};

use super::{
    ast::{Clause, Expr, Exprs, Var},
    helpers::{comma_sep_list, opt_annotation, ws},
    lex::{fname, lit},
    pat::pats,
    terminals::{atom, var},
    top::fun,
};

// TODO: Common pattern for nested list, avoid manual rewrite!
fn expr_nested_list(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("["))(i)?;
    let (i, expr) = ws(exprs)(i)?;
    let head = match expr {
        Exprs::Single(expr) => match *expr {
            Expr::List(exprs) => exprs,
            val => vec![Exprs::Single(Box::new(val))],
        },
        otherwise => vec![otherwise], // Value list should not be flatted
    };

    let (i, _) = ws(tag("|"))(i)?;
    let (i, expr) = ws(exprs)(i)?;
    let tail = match expr {
        Exprs::Single(expr) => match *expr {
            Expr::List(exprs) => exprs,
            val => vec![Exprs::Single(Box::new(val))],
        },
        otherwise => vec![otherwise], // Value list should not be flatted
    };
    let (i, _) = ws(tag("]"))(i)?;

    let cons = [&head[..], &tail[..]].concat();
    Ok((i, crate::parser::ast::Expr::List(cons)))
}

fn case_of(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("case"))(i)?;
    let (i, exprs) = exprs(i)?;
    let (i, _) = ws(tag("of"))(i)?;
    let (i, clauses) = many0(clause)(i)?;
    let (i, _) = ws(tag("end"))(i)?;
    Ok((i, crate::parser::ast::Expr::Case(exprs, clauses)))
}

fn letrec(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("letrec"))(i)?;
    let (i, fundefs) = many0(ws(fun))(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, expressions) = exprs(i)?;
    Ok((i, crate::parser::ast::Expr::LetRec(fundefs, expressions)))
}

fn apply(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("apply"))(i)?;
    let (i, exprs0) = ws(exprs)(i)?;
    let (i, exprs_args) = comma_sep_list("(", ")", ws(exprs))(i)?;
    Ok((i, crate::parser::ast::Expr::Apply(exprs0, exprs_args)))
}

fn call(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("call"))(i)?;
    let (i, module) = ws(exprs)(i)?;
    let (i, _) = ws(tag(":"))(i)?;
    let (i, name) = ws(exprs)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((i, crate::parser::ast::Expr::Call(module, name, args)))
}

fn primop(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("primop"))(i)?;
    let (i, name) = ws(atom)(i)?;
    let (i, args) = comma_sep_list("(", ")", exprs)(i)?;
    Ok((i, crate::parser::ast::Expr::PrimOp(name, args)))
}

fn receive(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("receive"))(i)?;
    let (i, clauses) = many0(clause)(i)?;
    let (i, _) = ws(tag("after"))(i)?;
    let (i, timeout) = ws(exprs)(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, action) = ws(exprs)(i)?;
    Ok((
        i,
        crate::parser::ast::Expr::Receive(clauses, timeout, action),
    ))
}

fn try_expr(i: &str) -> IResult<&str, Expr> {
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
        crate::parser::ast::Expr::Try(arg, vars_, body, evars, handler),
    ))
}

fn do_expr(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("do"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    let (i, exprs2) = ws(exprs)(i)?;
    Ok((i, crate::parser::ast::Expr::Do(exprs1, exprs2)))
}

fn catch(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("catch"))(i)?;
    let (i, exprs1) = ws(exprs)(i)?;
    Ok((i, crate::parser::ast::Expr::Catch(exprs1)))
}

// TODO: More elegant way to add opt_annotation.
fn clause(i: &str) -> IResult<&str, Clause> {
    opt_annotation(clause_inner)(i)
}

fn clause_inner(i: &str) -> IResult<&str, Clause> {
    let (i, pats) = pats(i)?;
    let (i, _) = ws(tag("when"))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("->"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((
        i,
        crate::parser::ast::Clause {
            pats,
            when: exprs1,
            res: exprs2,
        },
    ))
}

fn let_in(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(tag("let"))(i)?;
    // Delay thinking about how the vars and exprs match in the equal sign
    let (i, vars) = vars(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, exprs1) = exprs(i)?;
    let (i, _) = ws(tag("in"))(i)?;
    let (i, exprs2) = exprs(i)?;
    Ok((i, crate::parser::ast::Expr::Let(vars, exprs1, exprs2)))
}

fn vars(i: &str) -> IResult<&str, Vec<Var>> {
    opt_annotation(vars_inner)(i)
}

// TODO: Move? It is here in lack of a better fitting module
fn vars_inner(i: &str) -> IResult<&str, Vec<Var>> {
    alt((map(var, |o| vec![o]), comma_sep_list("<", ">", var)))(i)
}

fn expr(i: &str) -> IResult<&str, Expr> {
    println!("Hit single expr {}", i);
    opt_annotation(expr_inner)(i)
}

fn expr_inner(i: &str) -> IResult<&str, Expr> {
    ws(alt((
        map(var, crate::parser::ast::Expr::Var),
        map(fname, crate::parser::ast::Expr::Fname),
        map(lit, crate::parser::ast::Expr::Lit),
        map(fun, |fun| crate::parser::ast::Expr::Fun(fun)),
        expr_nested_list,
        map(
            comma_sep_list("[", "]", exprs),
            crate::parser::ast::Expr::List,
        ),
        map(
            comma_sep_list("{", "}", exprs),
            crate::parser::ast::Expr::Tuple,
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
    )))(i)
}

// TODO: Redundant opt_annotation here?
pub fn exprs(i: &str) -> IResult<&str, Exprs> {
    ws(alt((
        map(opt_annotation(expr), |o| {
            crate::parser::ast::Exprs::Single(Box::new(o))
        }),
        map(
            comma_sep_list("<", ">", opt_annotation(expr)),
            crate::parser::ast::Exprs::Values,
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
}
