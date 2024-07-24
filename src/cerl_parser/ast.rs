// Core Erlang AST as is.

use std::fmt;

// "New" types
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Atom {
    pub anno: Anno,
    pub name: String,
}

// TODO: Rename to type AnnoAtom, and use Atom instead of String where Annotation-free Atoms should be used.

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Var {
    pub anno: Anno,
    pub name: String,
}

pub struct Module {
    pub anno: Anno,
    pub inner: ModuleInner,
}

// AST
#[derive(Debug)]
pub struct ModuleInner {
    pub name: Atom,
    pub exports: Vec<FunName>,
    pub attributes: Vec<Attribute>,
    pub body: Vec<(FunName, FunDef)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunCall {
    PrimOp(Atom),       // Call basic erlang functions
    Apply(Exprs),       // Inter-module call // Note: Apparently fname is used here
    Call(Exprs, Exprs), // Cross-module call; (Module, Call name)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunName {
    pub anno: Anno,
    pub inner: FunNameInner,
}

// TODO: General all XxxInner should be renamed such that it is AnnoXxx and XxxInner is renamed to
// Xxx again. The Inner rename is to get help from the compiler to migrate towards annotations
// correctly.

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunNameInner {
    pub name: Atom,
    pub arity: usize,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Atom,
    pub value: Lit,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct FunDef {
    pub anno: Anno,
    pub inner: FunDefInner,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct FunDefInner {
    pub args: Vec<Var>,
    pub body: Exprs,
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
pub struct Lit {
    pub anno: Anno,
    pub inner: LitInner,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum LitInner {
    Int(i64),
    Float(Float),
    Atom(String), // Note: Atom cannot be annotated within a literal, therefore String.
    Char(char),
    Cons(Box<(Lit, Lit)>), // Note: Converted from head tail to vector in parser
    Tuple(Vec<Lit>),
    String(String),
    Nil, // Nil is intentionally left out. Cons is converted to vector, thus no need for nil.
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Anno(pub Option<LitInner>);

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum Exprs {
    One(Box<Expr>),
    Many(Vec<Expr>),
}

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Expr {
    pub anno: Anno,
    pub inner: ExprInner,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ExprInner {
    Var(Var),
    Fname(FunName), // Note fname is for e.g. 'foo'/1 = fun (X) -> 1+X end.
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
    Do(Exprs, Exprs),
    Catch(Exprs),
    Map(Vec<MapPair>, Option<Box<Expr>>), // TODO: More transparent way to allow "update" from map or variable that contains a map
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Clause {
    pub anno: Anno,
    pub inner: ClauseInner,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct ClauseInner {
    pub pats: Vec<Pat>,
    pub when: Exprs,
    pub res: Exprs,
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub struct Pat {
    pub anno: Anno,
    pub inner: PatInner,
}

// TODO: Can this be changed so case and let use same format for assignments???
// This structure makes code reuse much harder than it should be!
#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum PatInner {
    Var(Var),
    Lit(Lit),
    Cons(Box<(Pat, Pat)>),
    Tuple(Vec<Pat>),
    Alias(Var, Box<Pat>),
}
// TODO: Generic way to handle this "option anno" print out instead of manual copy-paste.
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
