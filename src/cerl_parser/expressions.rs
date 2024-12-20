use std::rc::Rc;

use nom::{
    branch::alt,
    combinator::{map, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag};

use super::{
    ast::{
        AnnoClause, AnnoExpr, AnnoFunName, AnnoMap, AnnoMapPair, AnnoPat, AnnoVar, CallModule,
        Clause, Expr, Fun, FunDef, FunLit, Lit, MapExpr, MapPair, MapPairType, Receive, Timeout,
        Try,
    },
    cinput::CInput,
    grammar::function_definitions,
    helpers::{comma_sep_list, cons, loc, opt_angle_bracket, opt_annotation, wsa},
    patterns::{anno_pattern, anno_variable},
    tokeniser::{atom, char_char, float, fname, integer, string, var},
};

// WSA OK
// Note: Do not wrap in Rc as AnnoExpr is used in Vec
fn anno_expression(i: CInput) -> IResult<CInput, AnnoExpr, ErrorTree<CInput>> {
    map(loc(opt_annotation(expression)), |(loc, (inner, anno))| {
        AnnoExpr { loc, anno, inner }
    })(i)
}

// WSA OK
fn expression(i: CInput) -> IResult<CInput, Rc<Expr>, ErrorTree<CInput>> {
    alt((
        map(
            map(loc(comma_sep_list("<", ">", anno_expression)), |(l, o)| {
                Expr::Exprs(l, o)
            }),
            |o| o.into(),
        ),
        single_expression,
    ))(i)
}

// WSA OK
fn single_expression(i: CInput) -> IResult<CInput, Rc<Expr>, ErrorTree<CInput>> {
    map(
        alt((
            map(loc(fname), |(l, o)| Expr::Fname(l, o)), // fname must have higher precedence than atomic_literal.
            map(loc(atomic_literal), |(l, o)| Expr::AtomLit(l, o.into())),
            map(loc(tuple_expression), |(l, o)| Expr::Tuple(l, o)),
            map(loc(cons_expression), |(l, o)| Expr::Cons(l, o)),
            // Ready for extension: binary
            map(loc(var), |(l, o)| Expr::Var(l, o)),
            map(loc(fun_literal), |(l, o)| Expr::FunLit(l, o)),
            map(loc(fun_expr), |(l, o)| Expr::Fun(l, o)),
            map(loc(letrec_expr), |(l, (defs, body))| {
                // letrec must have higher precedence than let to avoid "let" substring match in letrec
                Expr::LetRec(l, defs, body.into())
            }),
            map(loc(let_expr), |(l, (var, e1, e2))| {
                Expr::Let(l, var, e1.into(), e2.into())
            }),
            map(loc(case_expr), |(l, (arg, clauses))| {
                Expr::Case(l, arg.into(), clauses)
            }),
            map(loc(receive_expr), |(l, receive_expr)| {
                Expr::Receive(l, receive_expr.into())
            }),
            call_expr, // Merge apply, call and primop.
            map(loc(try_expr), |(l, o)| Expr::Try(l, o)),
            map(loc(sequence), |(l, (e1, e2))| Expr::Do(l, e1, e2)),
            map(loc(catch_expr), |(l, o)| Expr::Catch(l, o)),
            map(loc(map_expr), |(l, o)| Expr::Map(l, o)),
        )),
        |o| o.into(),
    )(i)
}

// WSA OK
// Do not Rc as AnnoFunName is used in Vec
pub fn anno_function_name(i: CInput) -> IResult<CInput, AnnoFunName, ErrorTree<CInput>> {
    map(loc(opt_annotation(fname)), |(loc, (inner, anno))| {
        AnnoFunName { loc, anno, inner }
    })(i)
}

// WSA OK
fn let_vars(i: CInput) -> IResult<CInput, Vec<AnnoVar>, ErrorTree<CInput>> {
    opt_angle_bracket(anno_variable)(i)
}

// WSA OK
fn sequence(i: CInput) -> IResult<CInput, (Rc<AnnoExpr>, Rc<AnnoExpr>), ErrorTree<CInput>> {
    map(
        preceded(wsa(tag("do")), pair(anno_expression, anno_expression)),
        |(o1, o2)| (o1.into(), o2.into()),
    )(i)
}

// WSA OK
pub fn fun_expr(i: CInput) -> IResult<CInput, Rc<Fun>, ErrorTree<CInput>> {
    map(
        loc(tuple((
            wsa(tag("fun")),
            comma_sep_list("(", ")", anno_variable),
            wsa(tag("->")),
            anno_expression,
        ))),
        |(loc, (_, vars, _, body))| {
            Fun {
                loc,
                vars,
                body: body.into(),
            }
            .into()
        },
    )(i)
}

// WSA OK
fn let_expr(i: CInput) -> IResult<CInput, (Vec<AnnoVar>, AnnoExpr, AnnoExpr), ErrorTree<CInput>> {
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
fn letrec_expr(i: CInput) -> IResult<CInput, (Vec<FunDef>, AnnoExpr), ErrorTree<CInput>> {
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
fn case_expr(i: CInput) -> IResult<CInput, (AnnoExpr, Vec<AnnoClause>), ErrorTree<CInput>> {
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
// Note: Do not Rc as AnnoClause is used in Vec
fn anno_clause(i: CInput) -> IResult<CInput, AnnoClause, ErrorTree<CInput>> {
    map(loc(opt_annotation(clause)), |(loc, (inner, anno))| {
        AnnoClause { loc, anno, inner }
    })(i)
}

// WSA OK
fn clause(i: CInput) -> IResult<CInput, Rc<Clause>, ErrorTree<CInput>> {
    map(
        loc(tuple((
            clause_pattern,
            wsa(tag("when")),
            anno_expression,
            wsa(tag("->")),
            anno_expression,
        ))),
        |(loc, (pats, _, when, _, res))| {
            Clause {
                loc,
                pats,
                when: when.into(),
                res: res.into(),
            }
            .into()
        },
    )(i)
}

// WSA OK
fn clause_pattern(i: CInput) -> IResult<CInput, Vec<AnnoPat>, ErrorTree<CInput>> {
    opt_angle_bracket(anno_pattern)(i)
}

// Merge apply, call and primop as they are very similar.
// WSA OK
fn call_expr(i: CInput) -> IResult<CInput, Expr, ErrorTree<CInput>> {
    map(
        loc(pair(
            alt((
                map(preceded(wsa(tag("apply")), anno_expression), |o| {
                    (CallModule::Apply, o.into())
                }),
                map(
                    preceded(
                        wsa(tag("call")),
                        tuple((anno_expression, wsa(tag(":")), anno_expression)),
                    ),
                    |(o1, _, o2)| (CallModule::Call(o1.into()), o2.into()),
                ),
                map(preceded(wsa(tag("primop")), anno_expression), |o| {
                    (CallModule::PrimOp, o.into())
                }),
            )),
            comma_sep_list("(", ")", anno_expression),
        )),
        |(l, ((m, c), a))| Expr::Call(l, m, c, a),
    )(i)
}

// WSA OK
fn try_expr(i: CInput) -> IResult<CInput, Rc<Try>, ErrorTree<CInput>> {
    map(
        loc(preceded(
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
        )),
        |(loc, (arg, _, vars, _, body, _, evars, _, handler))| {
            Try {
                loc,
                arg,
                vars,
                body,
                evars,
                handler,
            }
            .into()
        },
    )(i)
}

// WSA OK
fn catch_expr(i: CInput) -> IResult<CInput, Rc<AnnoExpr>, ErrorTree<CInput>> {
    map(preceded(wsa(tag("catch")), anno_expression), |o| o.into())(i)
}

// WSA OK
fn receive_expr(i: CInput) -> IResult<CInput, Receive, ErrorTree<CInput>> {
    map(
        loc(preceded(
            wsa(tag("receive")),
            tuple((many0(anno_clause), timeout_expr)),
        )),
        |(loc, (clauses, timeout))| Receive {
            loc,
            clauses,
            timeout,
        },
    )(i)
}

// WSA OK
fn timeout_expr(i: CInput) -> IResult<CInput, Timeout, ErrorTree<CInput>> {
    map(
        loc(preceded(
            wsa(tag("after")),
            pair(anno_expression, anno_expression),
        )),
        |(loc, (guard, action))| Timeout { loc, guard, action },
    )(i)
}

// WSA OK
// Note:: Do not Rc as Lit is used in Vec
pub fn literal(i: CInput) -> IResult<CInput, Lit, ErrorTree<CInput>> {
    alt((
        atomic_literal,
        map(tuple_literal, Lit::Tuple),
        map(cons_literal, Lit::Cons),
    ))(i)
}

// WSA OK
// Note: Do not Rc as Lit is used in Vec
pub fn atomic_literal(i: CInput) -> IResult<CInput, Lit, ErrorTree<CInput>> {
    alt((
        map(char_char, Lit::Char),
        map(float, Lit::Float),
        map(integer, Lit::Int),
        map(atom, Lit::Atom),
        map(string, Lit::String),
        value(Lit::Nil, pair(wsa(tag("[")), wsa(tag("]")))),
    ))(i)
}

// WSA OK
fn tuple_literal(i: CInput) -> IResult<CInput, Vec<Lit>, ErrorTree<CInput>> {
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
fn cons_literal(i: CInput) -> IResult<CInput, Vec<Lit>, ErrorTree<CInput>> {
    map(
        preceded(wsa(tag("[")), pair(literal, tail_literal)),
        |(head, tail)| {
            let mut acc = vec![head];
            acc.extend(tail);
            acc
        },
    )(i)
}

fn tail_literal(i: CInput) -> IResult<CInput, Vec<Lit>, ErrorTree<CInput>> {
    alt((
        value(vec![], wsa(tag("]"))),
        map(delimited(wsa(tag("|")), literal, wsa(tag("]"))), |next| {
            match next {
                // TODO: Loc thrown away here. Bad or acceptable?
                Lit::Cons(next) => next,
                _ => vec![next],
            }
        }),
        map(
            preceded(wsa(tag(",")), pair(literal, tail_literal)),
            |(head, tail)| {
                let mut acc = vec![head];
                acc.extend(tail);
                acc
            },
        ),
    ))(i)
}

// WSA OK
fn fun_literal(i: CInput) -> IResult<CInput, Rc<FunLit>, ErrorTree<CInput>> {
    map(
        loc(tuple((wsa(tag("fun")), atom, wsa(tag(":")), fname))),
        |(loc, (_, module, _, fname))| FunLit { loc, module, fname }.into(),
    )(i)
}

// WSA OK
fn tuple_expression(i: CInput) -> IResult<CInput, Vec<AnnoExpr>, ErrorTree<CInput>> {
    comma_sep_list("{", "}", anno_expression)(i)
}

// WSA OK
fn anno_map_expr(i: CInput) -> IResult<CInput, AnnoMap, ErrorTree<CInput>> {
    map(loc(opt_annotation(map_expr)), |(loc, (inner, anno))| {
        AnnoMap { loc, anno, inner }
    })(i)
}

// WSA OK
fn map_expr(i: CInput) -> IResult<CInput, MapExpr, ErrorTree<CInput>> {
    delimited(
        wsa(tag("~")),
        alt((
            map(loc(comma_sep_list("{", "}", anno_map_pair)), |(l, o)| {
                MapExpr::OnlyPairs(l, o)
            }),
            delimited(
                wsa(tag("{")),
                alt((
                    map(
                        loc(tuple((
                            separated_list0(wsa(tag(",")), anno_map_pair),
                            wsa(tag("|")),
                            anno_variable,
                        ))),
                        |(l, (pairs, _, var))| MapExpr::MapVar(l, pairs, var.into()),
                    ),
                    map(
                        loc(tuple((
                            separated_list0(wsa(tag(",")), anno_map_pair),
                            wsa(tag("|")),
                            anno_map_expr,
                        ))),
                        |(l, (pairs, _, e))| MapExpr::AnnoMap(l, pairs, e.into()),
                    ),
                )),
                wsa(tag("}")),
            ),
        )),
        wsa(tag("~")),
    )(i)
}

// WSA OK
fn anno_map_pair(i: CInput) -> IResult<CInput, AnnoMapPair, ErrorTree<CInput>> {
    map(loc(opt_annotation(map_pair)), |(loc, (inner, anno))| {
        AnnoMapPair { loc, anno, inner }
    })(i)
}

// WSA OK
fn map_pair(i: CInput) -> IResult<CInput, MapPair, ErrorTree<CInput>> {
    map(
        loc(tuple((
            anno_expression,
            alt((
                value(MapPairType::Assoc, wsa(tag("=>"))),
                value(MapPairType::Exact, wsa(tag(":="))),
            )),
            anno_expression,
        ))),
        |(loc, (key, op, value))| MapPair {
            loc,
            op,
            key,
            value,
        },
    )(i)
}

// WSA OK
fn cons_expression(i: CInput) -> IResult<CInput, Vec<AnnoExpr>, ErrorTree<CInput>> {
    cons(anno_expression)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_testing() {
        assert!(anno_clause(CInput::new("( <'false'> when 'true' -> ( apply 'recv$^0'/0 () -| ['dialyzer_ignore'] ) -| ['dialyzer_ignore'] )")).is_ok());
        assert!(expression(CInput::new("apply 'recv$^0'/0 ()")).is_ok());
        assert!(expression(CInput::new("case _3 of
    <'true'> when 'true' -> 'true'
    ( <'false'> when 'true' -> ( apply 'recv$^0'/0 () -| ['dialyzer_ignore'] ) -| ['dialyzer_ignore'] )
   end")).is_ok())
    }

    #[test]
    fn flatten_cons() {
        assert_eq!(
            literal(CInput::new("[1,2]")).unwrap().1,
            literal(CInput::new("[1|[2]]")).unwrap().1
        );
        assert_eq!(
            literal(CInput::new("[1,2,3]")).unwrap().1,
            literal(CInput::new("[1|[2|[3]]]")).unwrap().1
        );
        assert_eq!(
            literal(CInput::new("[1,2,3]")).unwrap().1,
            literal(CInput::new("[1,2|[3]]")).unwrap().1
        );
        assert_eq!(
            literal(CInput::new("[1,2,3]")).unwrap().1,
            literal(CInput::new("[1|[2,3]]")).unwrap().1
        );
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
