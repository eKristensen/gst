use std::rc::Rc;

use nom::{
    branch::alt,
    combinator::{fail, map, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{
        AnnoClause, AnnoExpr, AnnoFunName, AnnoMap, AnnoMapPair, AnnoPat, AnnoVar, Clause, Expr,
        Fun, FunCall, FunDef, FunLit, Lit, MapExpr, MapPair, MapPairType, Receive, Timeout, Try,
    },
    grammar::function_definitions,
    helpers::{comma_sep_list, cons, opt_angle_bracket, opt_annotation, wsa},
    patterns::{anno_pattern, anno_variable},
    tokeniser::{atom, char_char, float, fname, integer, string, var},
};

// WSA OK
fn anno_expression(i: &str) -> IResult<&str, AnnoExpr, ErrorTree<&str>> {
    map(opt_annotation(expression), |(inner, anno)| AnnoExpr {
        anno,
        inner,
    })(i)
}

// WSA OK
fn expression(i: &str) -> IResult<&str, Rc<Expr>, ErrorTree<&str>> {
    alt((
        map(
            map(comma_sep_list("<", ">", anno_expression), Expr::Exprs),
            |o| o.into(),
        ),
        single_expression,
    ))(i)
}

// WSA OK
fn single_expression(i: &str) -> IResult<&str, Rc<Expr>, ErrorTree<&str>> {
    map(
        alt((
            map(fname, Expr::Fname), // fname must have higher precedence than atomic_literal.
            map(atomic_literal, Expr::AtomLit),
            map(tuple_expression, Expr::Tuple),
            map(cons_expression, Expr::Cons),
            // Ready for extension: binary
            map(var, Expr::Var),
            map(fun_literal, Expr::FunLit),
            map(fun_expr, |fun_expr| Expr::Fun(fun_expr)),
            map(letrec_expr, |(defs, body)| {
                // letrec must have higher precedence than let to avoid "let" substring match in letrec
                Expr::LetRec(defs, body.into())
            }),
            map(let_expr, |(var, e1, e2)| {
                Expr::Let(var, e1.into(), e2.into())
            }),
            map(case_expr, |(arg, clauses)| Expr::Case(arg.into(), clauses)),
            map(receive_expr, |receive_expr| {
                Expr::Receive(receive_expr.into())
            }),
            map(call_expr, |(name, args)| Expr::Call(name, args)), // Merge apply, call and primop.
            map(try_expr, |try_expr| Expr::Try(Box::new(try_expr))),
            map(sequence, |(e1, e2)| Expr::Do(Box::new(e1), Box::new(e2))),
            map(catch_expr, |catch_expr| Expr::Catch(Box::new(catch_expr))),
            map(map_expr, Expr::Map),
        )),
        |o| o.into(),
    )(i)
}

// WSA OK
pub fn anno_function_name(i: &str) -> IResult<&str, AnnoFunName, ErrorTree<&str>> {
    map(opt_annotation(fname), |(inner, anno)| AnnoFunName {
        anno,
        inner,
    })(i)
}

// WSA OK
fn let_vars(i: &str) -> IResult<&str, Vec<AnnoVar>, ErrorTree<&str>> {
    opt_angle_bracket(anno_variable)(i)
}

// WSA OK
fn sequence(i: &str) -> IResult<&str, (AnnoExpr, AnnoExpr), ErrorTree<&str>> {
    preceded(wsa(tag("do")), pair(anno_expression, anno_expression))(i)
}

// WSA OK
pub fn fun_expr(i: &str) -> IResult<&str, Rc<Fun>, ErrorTree<&str>> {
    map(
        tuple((
            wsa(tag("fun")),
            comma_sep_list("(", ")", anno_variable),
            wsa(tag("->")),
            anno_expression,
        )),
        |(_, vars, _, body)| Fun { vars, body }.into(),
    )(i)
}

// WSA OK
fn let_expr(i: &str) -> IResult<&str, (Vec<AnnoVar>, AnnoExpr, AnnoExpr), ErrorTree<&str>> {
    map(
        tuple((
            wsa(tag("let")),
            let_vars,
            wsa(tag("=")),
            anno_expression,
            wsa(tag("in")),
            anno_expression,
        )),
        |(_, vars, _, e1, _, e2)| (vars, e1, e2),
    )(i)
}

// WSA OK
fn letrec_expr(i: &str) -> IResult<&str, (Vec<FunDef>, AnnoExpr), ErrorTree<&str>> {
    map(
        tuple((
            wsa(tag("letrec")),
            function_definitions,
            wsa(tag("in")),
            anno_expression,
        )),
        |(_, defs, _, body)| (defs, body),
    )(i)
}

// WSA OK
fn case_expr(i: &str) -> IResult<&str, (AnnoExpr, Vec<AnnoClause>), ErrorTree<&str>> {
    map(
        delimited(
            wsa(tag("case")),
            tuple((anno_expression, wsa(tag("of")), many1(anno_clause))),
            wsa(tag("end")),
        ),
        |(arg, _, clauses)| (arg, clauses),
    )(i)
}

// WSA OK
fn anno_clause(i: &str) -> IResult<&str, AnnoClause, ErrorTree<&str>> {
    map(opt_annotation(clause), |(inner, anno)| AnnoClause {
        anno,
        inner,
    })(i)
}

// WSA OK
fn clause(i: &str) -> IResult<&str, Clause, ErrorTree<&str>> {
    map(
        tuple((
            clause_pattern,
            wsa(tag("when")),
            anno_expression,
            wsa(tag("->")),
            anno_expression,
        )),
        |(pats, _, when, _, res)| Clause { pats, when, res },
    )(i)
}

// WSA OK
fn clause_pattern(i: &str) -> IResult<&str, Vec<AnnoPat>, ErrorTree<&str>> {
    opt_angle_bracket(anno_pattern)(i)
}

// Merge apply, call and primop as they are very similar.
// WSA OK
fn call_expr(i: &str) -> IResult<&str, (Rc<FunCall>, Vec<AnnoExpr>), ErrorTree<&str>> {
    pair(
        map(
            alt((
                map(preceded(wsa(tag("apply")), anno_expression), FunCall::Apply),
                map(
                    preceded(
                        wsa(tag("call")),
                        tuple((anno_expression, wsa(tag(":")), anno_expression)),
                    ),
                    |(o1, _, o2)| FunCall::Call(o1, o2),
                ),
                map(
                    preceded(wsa(tag("primop")), anno_expression),
                    FunCall::PrimOp,
                ),
            )),
            |o| o.into(),
        ),
        comma_sep_list("(", ")", anno_expression),
    )(i)
}

// WSA OK
fn try_expr(i: &str) -> IResult<&str, Try, ErrorTree<&str>> {
    map(
        preceded(
            wsa(tag("try")),
            tuple((
                anno_expression,
                wsa(tag("of")),
                let_vars,
                wsa(tag("->")),
                anno_expression,
                wsa(tag("catch")),
                let_vars,
                wsa(tag("->")),
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

// WSA OK
fn catch_expr(i: &str) -> IResult<&str, AnnoExpr, ErrorTree<&str>> {
    preceded(wsa(tag("catch")), anno_expression)(i)
}

// WSA OK
fn receive_expr(i: &str) -> IResult<&str, Receive, ErrorTree<&str>> {
    map(
        preceded(
            wsa(tag("receive")),
            tuple((many0(anno_clause), timeout_expr)),
        ),
        |(clauses, timeout)| Receive { clauses, timeout },
    )(i)
}

// WSA OK
fn timeout_expr(i: &str) -> IResult<&str, Timeout, ErrorTree<&str>> {
    map(
        preceded(wsa(tag("after")), pair(anno_expression, anno_expression)),
        |(guard, action)| Timeout { guard, action },
    )(i)
}

// WSA OK
pub fn literal(i: &str) -> IResult<&str, Lit, ErrorTree<&str>> {
    alt((
        atomic_literal,
        map(tuple_literal, Lit::Tuple),
        map(cons_literal, Lit::Cons),
    ))(i)
}

// WSA OK
pub fn atomic_literal(i: &str) -> IResult<&str, Rc<Lit>, ErrorTree<&str>> {
    map(
        alt((
            map(char_char, Lit::Char),
            map(float, Lit::Float),
            map(integer, Lit::Int),
            map(atom, Lit::Atom),
            map(string, Lit::String),
            value(Lit::Nil, pair(wsa(tag("[")), wsa(tag("]")))),
        )),
        |o| Rc::new(o),
    )(i)
}

// WSA OK
fn tuple_literal(i: &str) -> IResult<&str, Vec<Lit>, ErrorTree<&str>> {
    comma_sep_list("{", "}", literal)(i)
}

// WSA OK
// Cons Literal is special since it can be made into a flat list without any loss (no annotations)
// TODO: Optimize implementaiton. A bunch of manually repeated code in here
// Maybe a better idea is to flatten the list right away with normal cons and vector reconstruction
// And to use the same parser.
// TODO: New idea: Fold parser to build cons vector as values are known.
// Instead of reverting division, then add based on the type
// Fold = Vec, each new element added based on wheter they are Lit::Cons or not.
// It kinda could work. It would resolve the issue of reverting changes to an existing vector from
// the tail.
// Fold does NOT work. What to iterate over?
// BUT idea of accumulator is good, add it to tail function!
fn cons_literal(i: &str) -> IResult<&str, Vec<Lit>, ErrorTree<&str>> {
    let mut acc: Vec<Lit> = Vec::new();
    let (i, head) = preceded(wsa(tag("[")), literal)(i)?;
    acc.push(head);
    let (i, res) = tail_literal(acc, i)?;
    Ok((i, res))
}

fn tail_literal(mut acc: Vec<Lit>, i: &str) -> IResult<&str, Vec<Lit>, ErrorTree<&str>> {
    if let Ok((i, _)) = wsa(tag("]"))(i) {
        return Ok((i, acc));
    };

    if let Ok((i, next)) = delimited(wsa(tag("|")), literal, wsa(tag("]")))(i) {
        match next {
            Lit::Cons(next) => acc.extend(next),
            _ => acc.push(next),
        }
        return Ok((i, acc));
    };

    match preceded(wsa(tag(",")), literal)(i) {
        Ok((i, next)) => {
            acc.push(next);
            tail_literal(acc, i)
        }
        Err(_) => fail(i), // TODO: More informative err msg
    }
}

// WSA OK
fn fun_literal(i: &str) -> IResult<&str, Rc<FunLit>, ErrorTree<&str>> {
    map(
        tuple((wsa(tag("fun")), atom, wsa(tag(":")), fname)),
        |(_, module, _, fname)| FunLit { module, fname }.into(),
    )(i)
}

// WSA OK
fn tuple_expression(i: &str) -> IResult<&str, Vec<AnnoExpr>, ErrorTree<&str>> {
    comma_sep_list("{", "}", anno_expression)(i)
}

// WSA OK
fn anno_map_expr(i: &str) -> IResult<&str, AnnoMap, ErrorTree<&str>> {
    map(opt_annotation(map_expr), |(inner, anno)| AnnoMap {
        anno,
        inner,
    })(i)
}

// WSA OK
fn map_expr(i: &str) -> IResult<&str, MapExpr, ErrorTree<&str>> {
    delimited(
        wsa(tag("~")),
        alt((
            map(comma_sep_list("{", "}", anno_map_pair), MapExpr::OnlyPairs),
            delimited(
                wsa(tag("{")),
                alt((
                    map(
                        tuple((
                            separated_list0(wsa(tag(",")), anno_map_pair),
                            wsa(tag("|")),
                            anno_variable,
                        )),
                        |(pairs, _, var)| MapExpr::MapVar(pairs, var),
                    ),
                    map(
                        tuple((
                            separated_list0(wsa(tag(",")), anno_map_pair),
                            wsa(tag("|")),
                            anno_map_expr,
                        )),
                        |(pairs, _, e)| MapExpr::AnnoMap(pairs, e.into()),
                    ),
                )),
                wsa(tag("}")),
            ),
        )),
        wsa(tag("~")),
    )(i)
}

// WSA OK
fn anno_map_pair(i: &str) -> IResult<&str, AnnoMapPair, ErrorTree<&str>> {
    map(opt_annotation(map_pair), |(inner, anno)| AnnoMapPair {
        anno,
        inner,
    })(i)
}

// WSA OK
fn map_pair(i: &str) -> IResult<&str, MapPair, ErrorTree<&str>> {
    map(
        tuple((
            anno_expression,
            alt((
                value(MapPairType::Assoc, wsa(tag("=>"))),
                value(MapPairType::Exact, wsa(tag(":="))),
            )),
            anno_expression,
        )),
        |(key, op, value)| MapPair { op, key, value },
    )(i)
}

// WSA OK
fn cons_expression(i: &str) -> IResult<&str, Vec<AnnoExpr>, ErrorTree<&str>> {
    cons(anno_expression)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_testing() {
        assert!(anno_clause("( <'false'> when 'true' -> ( apply 'recv$^0'/0 () -| ['dialyzer_ignore'] ) -| ['dialyzer_ignore'] )").is_ok());
        assert!(expression("apply 'recv$^0'/0 ()").is_ok());
        assert!(expression("case _3 of
    <'true'> when 'true' -> 'true'
    ( <'false'> when 'true' -> ( apply 'recv$^0'/0 () -| ['dialyzer_ignore'] ) -| ['dialyzer_ignore'] )
   end").is_ok())
    }

    #[test]
    fn flatten_cons() {
        assert_eq!(literal("[1,2]").unwrap(), literal("[1|[2]]").unwrap());
        assert_eq!(literal("[1,2,3]").unwrap(), literal("[1|[2|[3]]]").unwrap());
        assert_eq!(literal("[1,2,3]").unwrap(), literal("[1,2|[3]]").unwrap());
        assert_eq!(literal("[1,2,3]").unwrap(), literal("[1|[2,3]]").unwrap());
    }
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
