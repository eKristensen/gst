// Core Erlang AST as is.

use std::{
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

// AST is based on: https://github.com/erlang/otp/blob/master/lib/compiler/src/core_parse.yrl
// The goal is to parse core erlang as close as possible to the reference implementation.

// Generic line column location
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Loc {
    pub pos: usize, // Very simple position to get started
                    // TODO: Convert to line and column numbers instead
                    // pub line: usize,
                    // pub column: usize,
}

// Core erlang specific to keep hints to original erl file.
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct CLoc {
    pub comment: Option<usize>, // Line number in comment inserted by erlc. Refers to original Erlang file.
    pub start: Loc,
    pub end: Loc,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Anno(pub Option<Vec<(CLoc, Const)>>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoAtom {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub name: Rc<Atom>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Atom(pub CLoc, pub String);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoVar {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub name: Rc<Var>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Var(pub CLoc, pub String);

#[derive(Debug, Clone)]
pub struct AnnoModule {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Module,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub loc: CLoc,
    pub name: Rc<Atom>,
    pub exports: Vec<AnnoFunName>,
    pub attributes: Vec<Attribute>,
    pub defs: Vec<FunDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub loc: CLoc,
    pub name: Rc<AnnoAtom>,
    pub value: Rc<AnnoLit>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CallModule {
    PrimOp,             // Call basic erlang functions
    Apply,              // Inter-module call // Note: Apparently fname is used here
    Call(Rc<AnnoExpr>), // Cross-module call; (Module, Call name)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoFunName {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Rc<FunName>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunName {
    pub loc: CLoc,
    pub name: Rc<Atom>, // Function name atom cannot be annotated.
    pub arity: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoFun {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub fun: Rc<Fun>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Fun {
    pub loc: CLoc,
    pub vars: Vec<AnnoVar>,
    pub body: Rc<AnnoExpr>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct FunDef {
    pub loc: CLoc,
    pub name: Rc<AnnoFunName>,
    pub body: Rc<AnnoFun>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Float {
    pub loc: CLoc,
    // Note: Why not store as float? Because floats do not implement Hash in Rust.
    //       Furthermore, this format is non-destructive - It represents exactly
    //       what the source code does.
    pub base: i64,
    pub decimal: u64,
    pub exponent: i64,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoLit {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Rc<Lit>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Lit {
    Int(CLoc, i64),
    Float(CLoc, Rc<Float>),
    Atom(CLoc, Rc<Atom>),
    Char(CLoc, char),
    Cons(CLoc, Vec<Lit>),
    Tuple(CLoc, Vec<Lit>),
    String(CLoc, String),
    Nil(CLoc),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Const(pub Lit);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MapPairType {
    Assoc,
    Exact,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoMapPair {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: MapPair,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MapPair {
    pub loc: CLoc,
    pub op: MapPairType,
    pub key: AnnoExpr,
    pub value: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoExpr {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Rc<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Exprs(CLoc, Vec<AnnoExpr>), // When expressions are in a "< e >" brackets
    Var(CLoc, Rc<Var>),
    Fname(CLoc, Rc<FunName>),
    AtomLit(CLoc, Rc<Lit>),
    FunLit(CLoc, Rc<FunLit>),
    Fun(CLoc, Rc<Fun>),
    Cons(CLoc, Vec<AnnoExpr>),
    Tuple(CLoc, Vec<AnnoExpr>),
    Let(CLoc, Vec<AnnoVar>, Rc<AnnoExpr>, Rc<AnnoExpr>), // Note: Let vars
    Case(CLoc, Rc<AnnoExpr>, Vec<AnnoClause>),
    LetRec(CLoc, Vec<FunDef>, Rc<AnnoExpr>),
    Call(CLoc, CallModule, Rc<AnnoExpr>, Vec<AnnoExpr>), // Merge call, apply and primop to avoid duplication
    Receive(CLoc, Rc<Receive>),
    Try(CLoc, Rc<Try>),
    Do(CLoc, Rc<AnnoExpr>, Rc<AnnoExpr>), // Sequence
    Catch(CLoc, Rc<AnnoExpr>),
    Map(CLoc, MapExpr), // TODO: More transparent way to allow "update" from map or variable that contains a map
                        // Ready for extensions: Binary, Segments
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MapExpr {
    OnlyPairs(CLoc, Vec<AnnoMapPair>),
    MapVar(CLoc, Vec<AnnoMapPair>, Rc<AnnoVar>),
    AnnoMap(CLoc, Vec<AnnoMapPair>, Rc<AnnoMap>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoMap {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: MapExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Try {
    pub loc: CLoc,
    pub arg: AnnoExpr,
    pub vars: Vec<AnnoVar>,
    pub body: AnnoExpr,
    pub evars: Vec<AnnoVar>, // Note: Let vars
    pub handler: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Receive {
    pub loc: CLoc,
    pub clauses: Vec<AnnoClause>,
    pub timeout: Timeout,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Timeout {
    pub loc: CLoc,
    pub guard: AnnoExpr,
    pub action: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunLit {
    pub loc: CLoc,
    pub module: Rc<Atom>,
    pub fname: Rc<FunName>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoClause {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Rc<Clause>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Clause {
    pub loc: CLoc,
    pub pats: Vec<AnnoPat>,
    pub when: Rc<AnnoExpr>,
    pub res: Rc<AnnoExpr>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoPat {
    pub loc: CLoc,
    pub anno: Rc<Anno>,
    pub inner: Rc<Pat>,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Pat {
    Var(CLoc, Rc<AnnoVar>),
    Lit(CLoc, Rc<Lit>),
    Cons(CLoc, Vec<AnnoPat>),
    Tuple(CLoc, Vec<AnnoPat>),
    Alias(CLoc, Rc<AnnoVar>, Rc<AnnoPat>),
    // Ready for extension: Maps, Segments, Binary
}

// TODO: Test display: Print parsed program, parse it again and expect same result in AST.

struct ConstList<'a>(&'a Vec<Const>);
struct ExportList<'a>(&'a Vec<AnnoFunName>);
struct AttributeList<'a>(&'a Vec<Attribute>);
struct DefList<'a>(&'a Vec<FunDef>);
struct AnnoVarList<'a>(&'a Vec<AnnoVar>);
struct AnnoExprList<'a>(&'a Vec<AnnoExpr>);
struct AnnoPatList<'a>(&'a Vec<AnnoPat>);
struct LetVars<'a>(&'a Vec<AnnoVar>);
struct AnnoClauseList<'a>(&'a Vec<AnnoClause>);

fn anno_fmt(f: &mut Formatter, anno: &Anno, inner: &impl Display) -> Result {
    match &anno {
        Anno(Some(anno)) => {
            let anno = anno.iter().map(|anno| anno.1.clone()).collect::<Vec<_>>();
            write!(f, "( '{}' -| {} )", inner, ConstList(&anno))
        }
        _ => inner.fmt(f),
    }
}

fn seperated_list(f: &mut Formatter, sep: &str, list: &Vec<impl Display>) -> Result {
    let mut first = true;
    for elm in list {
        if first {
            first = false;
        } else {
            write!(f, "{}", sep)?; // TODO: Do without {}
        }
        elm.fmt(f)?;
    }
    // TODO: Satisfy return type without empty print
    write!(f, "")
}

fn comma_sep(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    seperated_list(f, ", ", list)
}

fn newline_sep(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    seperated_list(f, "\n", list)
}

fn paren_list(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    write!(f, "( ")?;
    comma_sep(f, list)?;
    write!(f, " )")
}

fn square_list(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    write!(f, "[ ")?;
    comma_sep(f, list)?;
    write!(f, " ]")
}

fn curly_list(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    write!(f, "{{ ")?;
    comma_sep(f, list)?;
    write!(f, " }}")
}

fn angle_list(f: &mut Formatter, list: &Vec<impl Display>) -> Result {
    write!(f, "< ")?;
    comma_sep(f, list)?;
    write!(f, " >")
}

impl Display for ConstList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        square_list(f, self.0)
    }
}

impl Display for ExportList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        square_list(f, self.0)
    }
}

impl Display for AttributeList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        comma_sep(f, self.0)
    }
}

impl Display for DefList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        newline_sep(f, self.0)
    }
}

impl Display for AnnoClauseList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        newline_sep(f, self.0)
    }
}

impl Display for AnnoExprList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        paren_list(f, self.0)
    }
}

impl Display for AnnoVarList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        paren_list(f, self.0)
    }
}

impl Display for AnnoPatList<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        angle_list(f, self.0)
    }
}

impl Display for LetVars<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        angle_list(f, self.0)
    }
}

impl Display for AnnoAtom {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.name)
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.fmt(f)
    }
}

impl Display for AnnoLit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Lit::Int(_, i) => i.fmt(f),
            Lit::Float(_, x) => x.fmt(f),
            Lit::Atom(_, a) => a.fmt(f),
            Lit::Char(_, c) => c.fmt(f),
            Lit::String(_, s) => s.fmt(f),
            Lit::Nil(_) => write!(f, "[]"),
            Lit::Cons(_, cons) => square_list(f, cons),
            Lit::Tuple(_, tuple) => curly_list(f, tuple),
        }
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut Formatter) -> Result {
        // TODO: Is there a better more official way to print float?
        write!(f, "{}.{}e{}", self.base, self.decimal, self.exponent)
    }
}

impl Display for AnnoFunName {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for FunName {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "'{}'", self.1)
    }
}

impl Display for AnnoVar {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.name)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.1.fmt(f)
    }
}

impl Display for AnnoModule {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "module {} {} attributes {} {} end",
            self.name,
            ExportList(&self.exports),
            AttributeList(&self.attributes),
            DefList(&self.defs)
        )
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

impl Display for FunDef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} = {}", self.name, self.body)
    }
}

impl Display for AnnoFun {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.fun)
    }
}

impl Display for Fun {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "fun {} -> {}", AnnoVarList(&self.vars), self.body)
    }
}

impl Display for AnnoExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Expr::Exprs(_, exprs) => angle_list(f, exprs),
            Expr::Var(_, v) => v.fmt(f),
            Expr::Fname(_, fname) => fname.fmt(f),
            Expr::AtomLit(_, l) => l.fmt(f),
            Expr::FunLit(_, flit) => flit.fmt(f),
            Expr::Fun(_, fexpr) => (*fexpr).fmt(f),
            Expr::Cons(_, cons) => square_list(f, cons),
            Expr::Tuple(_, tuple) => curly_list(f, tuple),
            Expr::Let(_, vars, e1, e2) => write!(f, "let {} = {} in {}", LetVars(vars), e1, e2),
            Expr::Case(_, arg, clauses) => {
                write!(f, "case {} of {} end", *arg, AnnoClauseList(clauses))
            }
            Expr::LetRec(_, defs, body) => write!(f, "letrec {} in {}", DefList(defs), *body),
            Expr::Call(_, module, call, args) => {
                write!(f, "{}{} {}", module, *call, AnnoExprList(args))
            } // Merge call, apply and primop to avoid duplication
            Expr::Receive(_, receive) => (*receive).fmt(f),
            Expr::Try(_, t) => (*t).fmt(f),
            Expr::Do(_, e1, e2) => write!(f, "do {} {}", *e1, *e2),
            Expr::Catch(_, e) => write!(f, "catch {}", *e),
            Expr::Map(_, m) => m.fmt(f),
        }
    }
}

impl Display for FunLit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "fun {}:{}", self.module, self.fname)
    }
}

impl Display for AnnoClause {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{} when {} -> {}",
            AnnoPatList(&self.pats),
            self.when,
            self.res
        )
    }
}

impl Display for CallModule {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            CallModule::Call(module) => write!(f, "call {}:", module),
            CallModule::Apply => write!(f, "apply "),
            CallModule::PrimOp => write!(f, "primop "),
        }
    }
}

impl Display for Receive {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "receive {} {}",
            AnnoClauseList(&self.clauses),
            self.timeout,
        )
    }
}

impl Display for Timeout {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "after {} -> {}", self.guard, self.action)
    }
}

impl Display for Try {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "try {} of {} -> {} catch {} -> {}",
            self.arg,
            LetVars(&self.vars),
            self.body,
            LetVars(&self.evars),
            self.handler
        )
    }
}

impl Display for AnnoMap {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for MapExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "~{{")?;
        match &self {
            MapExpr::OnlyPairs(_, map_pairs) => {
                comma_sep(f, map_pairs)?;
            }
            MapExpr::MapVar(_, map_pairs, var) => {
                comma_sep(f, map_pairs)?;
                write!(f, "|{}", var)?;
            }
            MapExpr::AnnoMap(_, map_pairs, anno_map_expr) => {
                comma_sep(f, map_pairs)?;
                write!(f, "|{}", anno_map_expr)?;
            }
        };
        write!(f, "}}~")
    }
}

impl Display for AnnoMapPair {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for MapPair {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} {} {}", self.key, self.op, self.value)
    }
}

impl Display for MapPairType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            MapPairType::Assoc => write!(f, "=>"),
            MapPairType::Exact => write!(f, ":="),
        }
    }
}

impl Display for AnnoPat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.inner)
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Pat::Var(_, v) => v.fmt(f),
            Pat::Lit(_, l) => l.fmt(f),
            Pat::Cons(_, cons) => square_list(f, cons),
            Pat::Tuple(_, tuple) => curly_list(f, tuple),
            Pat::Alias(_, var, pat) => write!(f, "{} = {}", var, pat),
        }
    }
}
