use std::fmt;

// "New" types
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Var(pub String);

// AST
#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub exports: Vec<FunName>,
    pub attributes: Vec<Attribute>,
    pub body: Vec<(FunName, FunDef)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunCall {
    pub kind: FunKind,
    pub name: Atom,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunName {
    pub name: Atom,
    pub arity: u64,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunKind {
    PrimOp,
    Apply,
    Call(Box<Exprs>),
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Atom,
    pub value: Lit,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct FunDef {
    pub args: Vec<Var>,
    pub body: Exprs,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Float {
    // Comment: Why not store as float? Because floats do not implement Hash in Rust.
    pub base: i64,
    pub decimal: u64,
    pub exponent: i64
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Lit {
    Int(i64),
    Float(Float),
    Atom(Atom),
    Char(String), // TODO note down: Wut? A char is not a char, It could be e.g. $\101
    Cons(Vec<Lit>), // TODO Would it be better to make head and tail like source code is?
    Tuple(Vec<Lit>),
    String(String),
    Nil,
}

// Parser will technically interpret X alone as <X>
// TODO: Change if this becomes a problem
#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Exprs(pub Vec<Expr>);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MapPairType {
    Assoc,
    Exact,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MapPair {
    pub pair_type: MapPairType,
    pub key: Expr,
    pub value: Expr,
}

// TODO: Maybe it is a bad idea to use Vec<Expr> instead of having Exprs
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Var(Var),
    Fname(FunName), // Note fname is for e.g. 'foo/1 = fun (X) -> 1+X end.
    Lit(Lit),
    Fun(FunDef),
    Cons(Vec<Exprs>),
    Tuple(Vec<Exprs>),
    Let(Vec<Var>, Exprs, Exprs),
    Case(Exprs, Vec<Clause>),
    LetRec(Vec<(FunName, FunDef)>, Exprs),
    Call(FunCall, Vec<Exprs>), // Merge call, apply and primop to avoid duplication
    Receive(Vec<Clause>, Exprs, Exprs),
    Try(Exprs, Vec<Var>, Exprs, Vec<Var>, Exprs),
    Do(Exprs, Exprs), // TODO: Maybe merge into one Expr list ?
    Catch(Exprs),
    Map(Vec<MapPair>, Option<Box<Expr>>), // TODO: More transparent way to allow "update" from map or variable that contains a map
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Clause {
    pub pats: Vec<Pat>,
    pub when: Exprs,
    pub res: Exprs,
}

// TODO: Can this be changed so case and let use same format for assignments???
// This structure makes code reuse much harder than it should be!
#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Pat {
    Var(Var),
    Lit(Lit),
    Cons(Vec<Pat>),
    Tuple(Vec<Pat>),
    Alias(Var, Box<Pat>),
}

impl fmt::Display for FunName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
