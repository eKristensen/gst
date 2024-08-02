// Core Erlang AST as is.

use std::fmt::{Display, Formatter, Result};

// AST is based on: https://github.com/erlang/otp/blob/master/lib/compiler/src/core_parse.yrl
// The goal is to parse core erlang as close as possible to the reference implementation.

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Anno(pub Option<Vec<Const>>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoAtom {
    pub anno: Anno,
    pub name: Atom,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoVar {
    pub anno: Anno,
    pub name: Var,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone)]
pub struct AnnoModule {
    pub anno: Anno,
    pub inner: Module,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Atom,
    pub exports: Vec<AnnoFunName>,
    pub attributes: Vec<Attribute>,
    pub defs: Vec<FunDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: AnnoAtom,
    pub value: AnnoLit,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunCall {
    PrimOp(AnnoExpr),         // Call basic erlang functions
    Apply(AnnoExpr),          // Inter-module call // Note: Apparently fname is used here
    Call(AnnoExpr, AnnoExpr), // Cross-module call; (Module, Call name)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoFunName {
    pub anno: Anno,
    pub inner: FunName,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunName {
    pub name: Atom, // Function name atom cannot be annotated.
    pub arity: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoFun {
    pub anno: Anno,
    pub fun: Fun,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Fun {
    pub vars: Vec<AnnoVar>,
    pub body: AnnoExpr,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct FunDef {
    pub name: AnnoFunName,
    pub body: AnnoFun,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Float {
    // Note: Why not store as float? Because floats do not implement Hash in Rust.
    //       Furthermore, this format is non-destructive - It represents exactly
    //       what the source code does.
    pub base: i64,
    pub decimal: u64,
    pub exponent: i64,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoLit {
    pub anno: Anno,
    pub inner: Lit,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Lit {
    Int(i64),
    Float(Float),
    Atom(Atom),
    Char(char),
    Cons(Vec<Lit>),
    Tuple(Vec<Lit>),
    String(String),
    Nil,
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
    pub anno: Anno,
    pub inner: MapPair,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MapPair {
    pub op: MapPairType,
    pub key: AnnoExpr,
    pub value: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoExpr {
    pub anno: Anno,
    pub inner: Expr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Exprs(Vec<AnnoExpr>), // When expressions are in a "< e >" brackets
    Var(Var),
    Fname(FunName),
    AtomLit(Lit),
    FunLit(FunLit),
    Fun(Box<Fun>),
    Cons(Vec<AnnoExpr>),
    Tuple(Vec<AnnoExpr>),
    Let(Vec<AnnoVar>, Box<AnnoExpr>, Box<AnnoExpr>), // Note: Let vars
    Case(Box<AnnoExpr>, Vec<AnnoClause>),
    LetRec(Vec<FunDef>, Box<AnnoExpr>),
    Call(Box<FunCall>, Vec<AnnoExpr>), // Merge call, apply and primop to avoid duplication
    Receive(Box<Receive>),
    Try(Box<Try>),
    Do(Box<AnnoExpr>, Box<AnnoExpr>), // Sequence
    Catch(Box<AnnoExpr>),
    Map(MapExpr), // TODO: More transparent way to allow "update" from map or variable that contains a map
                  // Ready for extensions: Binary, Segments
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MapExpr {
    OnlyPairs(Vec<AnnoMapPair>),
    MapVar(Vec<AnnoMapPair>, AnnoVar),
    AnnoMap(Vec<AnnoMapPair>, Box<AnnoMap>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoMap {
    pub anno: Anno,
    pub inner: MapExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Try {
    pub arg: AnnoExpr,
    pub vars: Vec<AnnoVar>,
    pub body: AnnoExpr,
    pub evars: Vec<AnnoVar>, // Note: Let vars
    pub handler: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Receive {
    pub clauses: Vec<AnnoClause>,
    pub timeout: Timeout,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Timeout {
    pub guard: AnnoExpr,
    pub action: AnnoExpr,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunLit {
    pub module: Atom,
    pub fname: FunName,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoClause {
    pub anno: Anno,
    pub inner: Clause,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Clause {
    pub pats: Vec<AnnoPat>,
    pub when: AnnoExpr,
    pub res: AnnoExpr,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct AnnoPat {
    pub anno: Anno,
    pub inner: Pat,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Pat {
    Var(AnnoVar),
    Lit(Lit),
    Cons(Vec<AnnoPat>),
    Tuple(Vec<AnnoPat>),
    Alias(AnnoVar, Box<AnnoPat>),
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
        Anno(Some(anno)) => write!(f, "( '{}' -| {} )", inner, ConstList(anno)),
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
            Lit::Int(i) => i.fmt(f),
            Lit::Float(x) => x.fmt(f),
            Lit::Atom(a) => a.fmt(f),
            Lit::Char(c) => c.fmt(f),
            Lit::String(s) => s.fmt(f),
            Lit::Nil => write!(f, "[]"),
            Lit::Cons(cons) => square_list(f, cons),
            Lit::Tuple(tuple) => curly_list(f, tuple),
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
        write!(f, "'{}'", self.0)
    }
}

impl Display for AnnoVar {
    fn fmt(&self, f: &mut Formatter) -> Result {
        anno_fmt(f, &self.anno, &self.name)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.fmt(f)
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
            Expr::Exprs(exprs) => angle_list(f, exprs),
            Expr::Var(v) => v.fmt(f),
            Expr::Fname(fname) => fname.fmt(f),
            Expr::AtomLit(l) => l.fmt(f),
            Expr::FunLit(flit) => flit.fmt(f),
            Expr::Fun(fexpr) => (*fexpr).fmt(f),
            Expr::Cons(cons) => square_list(f, cons),
            Expr::Tuple(tuple) => curly_list(f, tuple),
            Expr::Let(vars, e1, e2) => write!(f, "let {} = {} in {}", LetVars(vars), e1, e2),
            Expr::Case(arg, clauses) => {
                write!(f, "case {} of {} end", *arg, AnnoClauseList(clauses))
            }
            Expr::LetRec(defs, body) => write!(f, "letrec {} in {}", DefList(defs), *body),
            Expr::Call(call, args) => write!(f, "{} {}", *call, AnnoExprList(args)), // Merge call, apply and primop to avoid duplication
            Expr::Receive(receive) => (*receive).fmt(f),
            Expr::Try(t) => (*t).fmt(f),
            Expr::Do(e1, e2) => write!(f, "do {} {}", *e1, *e2),
            Expr::Catch(e) => write!(f, "catch {}", *e),
            Expr::Map(m) => m.fmt(f),
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

impl Display for FunCall {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            FunCall::Call(module, call) => write!(f, "call {}:{}", module, call),
            FunCall::Apply(call) => write!(f, "apply {}", call),
            FunCall::PrimOp(call) => write!(f, "primop {}", call),
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
            MapExpr::OnlyPairs(map_pairs) => {
                comma_sep(f, map_pairs)?;
            }
            MapExpr::MapVar(map_pairs, var) => {
                comma_sep(f, map_pairs)?;
                write!(f, "|{}", var)?;
            }
            MapExpr::AnnoMap(map_pairs, anno_map_expr) => {
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
            Pat::Var(v) => v.fmt(f),
            Pat::Lit(l) => l.fmt(f),
            Pat::Cons(cons) => square_list(f, cons),
            Pat::Tuple(tuple) => curly_list(f, tuple),
            Pat::Alias(var, pat) => write!(f, "{} = {}", var, pat),
        }
    }
}
