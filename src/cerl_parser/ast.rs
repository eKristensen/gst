// Core Erlang AST as is.

//use std::fmt;

// AST is based on: https://github.com/erlang/otp/blob/master/lib/compiler/src/core_parse.yrl
// The goal is to parse core erlang as close as possible to the reference implementation.

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Anno {
    Unknown, // Depending on parser state it may be unknown wheter there is a annotation or not.
    None,
    Some(Vec<Const>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoAtom {
    pub anno: Anno,
    pub name: Atom,
}

pub struct Atom(pub String);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnnoVar {
    pub anno: Anno,
    pub name: Var,
}
pub struct Var(pub String);

#[derive(Debug)]
pub struct Module {
    pub anno: Anno,
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

pub struct AnnoFun {
    pub anno: Anno,
    pub fun: FunExpr,
}

pub struct FunExpr {
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

pub struct Const(pub Lit);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MapPairType {
    Assoc,
    Exact,
}

pub struct AnnoMapPair {
    pub anno: Anno,
    pub inner: MapPair,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MapPair {
    pub op: MapPairType,
    pub key: Expr,
    pub value: Expr,
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
    FunExpr(Box<FunExpr>),
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

pub enum MapExpr {
    OnlyPairs(Vec<AnnoMapPair>),
    MapVar(Vec<AnnoMapPair>, AnnoVar),
    AnnoMapExpr(Vec<AnnoMapPair>, Box<AnnoMapExpr>),
}

pub struct AnnoMapExpr {
    anno: Anno,
    inner: MapExpr,
}

pub struct Try {
    pub arg: AnnoExpr,
    pub vars: Vec<AnnoVar>,
    pub body: AnnoExpr,
    pub evars: Vec<AnnoVar>, // Note: Let vars
    pub handler: AnnoExpr,
}

pub struct Receive {
    pub clauses: Vec<AnnoClause>,
    pub timeout: AnnoExpr,
    pub action: AnnoExpr,
}

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
    pub pats: AnnoPat,
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
// TODO: Generic way to handle this "option anno" print out instead of manual copy-paste.
// TODO: Reimplement fmt display for ast
/*
impl fmt::Display for FunName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.anno.0 {
            None => write!(f, "'{}'", self.inner),
            Some(anno) => write!(f, "( '{}' -| {} )", self.inner, anno),
        }
    }
}

impl fmt::Display for FunNameInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

// TODO: Generic way to handle this "option anno" print out instead of manual copy-paste.
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.anno.0 {
            None => write!(f, "'{}'", self.name),
            Some(anno) => write!(f, "( '{}' -| {} )", self.name, anno),
        }
    }
}

// TODO: Generic way to handle this "option anno" print out instead of manual copy-paste.
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.anno.0 {
            None => write!(f, "{}", self.name),
            Some(anno) => write!(f, "( {} -| {} )", self.name, anno),
        }
    }
}

// TODO: Generic way to handle this "option anno" print out instead of manual copy-paste.
impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.anno.0 {
            None => write!(f, "{}", self.inner),
            Some(anno) => write!(f, "( {} -| {} )", self.inner, anno),
        }
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: Is there a better more official way to print float?
        write!(f, "{}.{}e{}", self.base, self.decimal, self.exponent)
    }
}

impl fmt::Display for LitInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LitInner::Int(i) => write!(f, "{}", i),
            LitInner::Float(x) => write!(f, "{}", x),
            LitInner::Atom(s) => write!(f, "'{}'", s),
            LitInner::Char(c) => write!(f, "{}", c),
            LitInner::String(s) => write!(f, "{}", s),
            LitInner::Nil => write!(f, "[]"),
            LitInner::Cons(head_tail_box) => {
                let (head, tail) = head_tail_box.as_ref();
                write!(f, "[{}|{}]", head, tail)
            }
            LitInner::Tuple(tl) => {
                write!(f, "{{");
                let mut first = true;
                // TODO: Avoid manuel repeat of this loop.
                for elm in tl {
                    if first {
                        first = false;
                    } else {
                        write!(f, " ,");
                    }
                    write!(f, "{}", elm);
                }

                write!(f, "}}")
            }
        }
    }
}
*/
