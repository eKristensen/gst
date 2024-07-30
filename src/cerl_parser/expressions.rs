use nom::{
    branch::alt,
    combinator::{map, value},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{
        AnnoClause, AnnoExpr, AnnoFunName, AnnoMapPair, AnnoPat, AnnoVar, Clause, Expr, FunCall,
        FunDef, FunExpr, FunLit, Lit, MapExpr, MapPair, MapPairType, Receive, Try,
    },
    grammar::function_definitions,
    helpers::{comma_sep_list, cons, opt_angle_bracket, opt_annotation, ws},
    patterns::{anno_pattern, anno_variable},
    tokeniser::{atom, char_char, float, fname, integer, string, var},
};

fn anno_expression(i: &str) -> IResult<&str, AnnoExpr, ErrorTree<&str>> {
    map(opt_annotation(expression), |(inner, anno)| AnnoExpr {
        anno,
        inner,
    })(i)
}

fn expression(i: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    alt((
        map(ws(comma_sep_list("<", ">", anno_expression)), Expr::Exprs),
        ws(single_expression),
    ))(i)
}

fn single_expression(i: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    alt((
        map(atomic_literal, Expr::AtomLit),
        map(tuple_expression, Expr::Tuple),
        map(cons_expression, Expr::Cons),
        // Ready for extension: binary
        map(var, Expr::Var),
        map(fname, Expr::Fname),
        map(fun_literal, Expr::FunLit),
        map(fun_expr, |fun_expr| Expr::FunExpr(Box::new(fun_expr))),
        map(let_expr, |(var, e1, e2)| {
            Expr::Let(var, Box::new(e1), Box::new(e2))
        }),
        map(letrec_expr, |(defs, body)| {
            Expr::LetRec(defs, Box::new(body))
        }),
        map(case_expr, |(arg, clauses)| {
            Expr::Case(Box::new(arg), clauses)
        }),
        map(receive_expr, |receive_expr| {
            Expr::Receive(Box::new(receive_expr))
        }),
        map(call_expr, |(name, args)| Expr::Call(Box::new(name), args)), // Merge apply, call and primop.
        map(try_expr, |try_expr| Expr::Try(Box::new(try_expr))),
        map(sequence, |(e1, e2)| Expr::Do(Box::new(e1), Box::new(e2))),
        map(catch_expr, |catch_expr| Expr::Catch(Box::new(catch_expr))),
        map(map_expr, Expr::Map),
    ))(i)
}

pub fn anno_function_name(i: &str) -> IResult<&str, AnnoFunName, ErrorTree<&str>> {
    map(opt_annotation(fname), |(inner, anno)| AnnoFunName {
        anno,
        inner,
    })(i)
}

fn let_vars(i: &str) -> IResult<&str, Vec<AnnoVar>, ErrorTree<&str>> {
    opt_angle_bracket(anno_variable)(i)
}

fn sequence(i: &str) -> IResult<&str, (AnnoExpr, AnnoExpr), ErrorTree<&str>> {
    preceded(ws(tag("do")), pair(anno_expression, anno_expression))(i)
}

pub fn fun_expr(i: &str) -> IResult<&str, FunExpr, ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("fun")),
            comma_sep_list("(", ")", anno_variable),
            ws(tag("->")),
            anno_expression,
        )),
        |(_, vars, _, body)| FunExpr { vars, body },
    )(i)
}

fn let_expr(i: &str) -> IResult<&str, (Vec<AnnoVar>, AnnoExpr, AnnoExpr), ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("let")),
            let_vars,
            ws(tag("=")),
            anno_expression,
            ws(tag("in")),
            anno_expression,
        )),
        |(_, vars, _, e1, _, e2)| (vars, e1, e2),
    )(i)
}

fn letrec_expr(i: &str) -> IResult<&str, (Vec<FunDef>, AnnoExpr), ErrorTree<&str>> {
    map(
        tuple((
            ws(tag("letrec")),
            function_definitions,
            ws(tag("in")),
            anno_expression,
        )),
        |(_, defs, _, body)| (defs, body),
    )(i)
}

fn case_expr(i: &str) -> IResult<&str, (AnnoExpr, Vec<AnnoClause>), ErrorTree<&str>> {
    map(
        delimited(
            ws(tag("case")),
            tuple((anno_expression, ws(tag("of")), many1(ws(anno_clause)))),
            ws(tag("end")),
        ),
        |(arg, _, clauses)| (arg, clauses),
    )(i)
}

fn anno_clause(i: &str) -> IResult<&str, AnnoClause, ErrorTree<&str>> {
    map(opt_annotation(clause), |(inner, anno)| AnnoClause {
        anno,
        inner,
    })(i)
}

fn clause(i: &str) -> IResult<&str, Clause, ErrorTree<&str>> {
    map(
        tuple((
            clause_pattern,
            ws(tag("when")),
            anno_expression,
            ws(tag("->")),
            anno_expression,
        )),
        |(pats, _, when, _, res)| Clause { pats, when, res },
    )(i)
}

fn clause_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    opt_angle_bracket(anno_pattern)(i)
}

// Merge apply, call and primop as they are very similar.
fn call_expr(i: &str) -> IResult<&str, (FunCall, Vec<AnnoExpr>), ErrorTree<&str>> {
    pair(
        alt((
            map(preceded(ws(tag("apply")), anno_expression), FunCall::PrimOp),
            map(
                preceded(
                    ws(tag("call")),
                    tuple((anno_expression, ws(tag(":")), anno_expression)),
                ),
                |(o1, _, o2)| FunCall::Call(o1, o2),
            ),
            map(
                preceded(ws(tag("primop")), anno_expression),
                FunCall::PrimOp,
            ),
        )),
        ws(comma_sep_list("(", ")", anno_expression)),
    )(i)
}

fn try_expr(i: &str) -> IResult<&str, Try, ErrorTree<&str>> {
    map(
        preceded(
            ws(tag("try")),
            tuple((
                anno_expression,
                ws(tag("of")),
                let_vars,
                ws(tag("->")),
                anno_expression,
                ws(tag("catch")),
                let_vars,
                ws(tag("->")),
                anno_expression,
            )),
        ),
        |(arg, _, vars, _, body, _, evars, _, handler)| Try {
            arg,
            vars,
            body,
            evars,
            handler,
        },
    )(i)
}

fn catch_expr(i: &str) -> IResult<&str, AnnoExpr, ErrorTree<&str>> {
    preceded(ws(tag("catch")), anno_expression)(i)
}

fn receive_expr(i: &str) -> IResult<&str, Receive, ErrorTree<&str>> {
    map(
        preceded(
            ws(tag("receive")),
            tuple((many0(anno_clause), anno_expression, anno_expression)),
        ),
        |(clauses, timeout, action)| Receive {
            clauses,
            timeout,
            action,
        },
    )(i)
}

pub fn literal(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    alt((
        atomic_literal,
        map(tuple_literal, Lit::Tuple),
        map(cons_literal, Lit::Cons),
    ))(i)
}

pub fn atomic_literal(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    alt((
        map(char_char, Lit::Char),
        map(integer, Lit::Int),
        map(float, Lit::Float),
        map(atom, Lit::Atom),
        map(string, Lit::String),
        value(Lit::Nil, pair(ws(tag("[")), ws(tag("]")))),
    ))(i)
}

fn tuple_literal(i: &str) -> IResult<&str, Vec<Lit>, ErrorTree<&str>> {
    ws(comma_sep_list("{", "}", literal))(i)
}

fn cons_literal(i: &str) -> IResult<&str, Vec<Lit>, ErrorTree<&str>> {
    cons(literal)(i)
}

fn fun_literal(i: &str) -> IResult<&str, FunLit, ErrorTree<&str>> {
    map(
        tuple((ws(tag("fun")), atom, ws(tag(":")), fname)),
        |(_, module, _, fname)| FunLit { module, fname },
    )(i)
}

fn tuple_expression(i: &str) -> IResult<&str, Vec<AnnoExpr>, ErrorTree<&str>> {
    ws(comma_sep_list("{", "}", anno_expression))(i)
}

fn map_expr(i: &str) -> IResult<&str, MapExpr, ErrorTree<&str>> {
    delimited(
        ws(tag("~")),
        alt((
            map(comma_sep_list("{", "}", anno_map_pair), MapExpr::OnlyPairs),
            // TODO: Map anno variable, map anno expr
        )),
        ws(tag("~")),
    )(i)
}

fn anno_map_pair(i: &str) -> IResult<&str, AnnoMapPair, ErrorTree<&str>> {
    map(opt_annotation(map_pair), |(inner, anno)| AnnoMapPair {
        anno,
        inner,
    })(i)
}

fn map_pair(i: &str) -> IResult<&str, MapPair, ErrorTree<&str>> {
    map(
        tuple((
            anno_expression,
            alt((
                value(MapPairType::Assoc, ws(tag("=>"))),
                value(MapPairType::Exact, ws(tag(":="))),
            )),
            anno_expression,
        )),
        |(key, op, value)| MapPair { op, key, value },
    )(i)
}

fn cons_expression(i: &str) -> IResult<&str, Vec<AnnoExpr>, ErrorTree<&str>> {
    cons(anno_expression)(i)
}

#[cfg(test)]
mod tests {
    //use super::*;

    // Test case where the issue was the args list. Wrapping for completeness
    // TODO: Check output maybe?
    /*    #[test]
      fn test_exprs_call_args_list() {
          assert!(
              exprs(" case call 'a':'b' (_0, 'new', 'neg') of <_2> when 'true' -> [] end ").is_ok()
          );
      }
    */
    /*
    TODO: Test this

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    'init'/1 =
        fun (_0) ->
          {'ok',{'plus_state',_0,'undefined',~{}~}}
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    */
}
