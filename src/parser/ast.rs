// TODO: Refactor to be more like the core erlang paper suggests the core erlang ast should look. Parsing is more important for now though.

// "New" types
#[derive(Debug, Clone)]
pub struct Fname(pub Atom);

#[derive(Debug, Clone)]
pub struct Atom(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct Integer(pub i64);

#[derive(Debug, Clone)]
pub struct Var(pub String);

// AST
#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub exports: Vec<FunHead>,
    pub attributes: Vec<Attribute>,
    pub body: Vec<FunDef>,
}

#[derive(Debug, Clone)]
pub struct FunHead {
    pub name: Fname,
    pub arity: Integer, // TODO: Hmm Integer here does allow arity to be negative.....
}

#[derive(Debug)]
pub struct Attribute {
    pub name: Atom,
    pub value: Const,
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub head: FunHead,
    pub args: Vec<Var>,
    pub body: Exprs,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(Integer),
    Float(f32),
    Atom(Atom),
    Char(char),
    List(Vec<Const>),  // TODO: Implement parser
    Tuple(Vec<Const>), // TODO: Implement parser
    String(String),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Const {
    Lit(Lit),
    List(Vec<Const>),
    Tuple(Vec<Const>),
}

#[derive(Debug, Clone)]
pub enum Exprs {
    Single(Box<Expr>),
    Values(Vec<Expr>),
}

// TODO: Maybe it is a bad idea to use Vec<Expr> instead of having Exprs
#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Fname(FunHead),
    Lit(Lit),
    Fun(FunDef),
    List(Vec<Exprs>),
    Tuple(Vec<Exprs>),
    Let(Vec<Var>, Exprs, Exprs),
    Case(Exprs, Vec<Clause>),
    LetRec(Vec<FunDef>, Exprs),
    Apply(Exprs, Vec<Exprs>),
    Call(Exprs, Exprs, Vec<Exprs>),
    PrimOp(Atom, Vec<Exprs>),
    Receive(Vec<Clause>, Exprs, Exprs),
    Try(Exprs, Vec<Var>, Exprs, Vec<Var>, Exprs),
    Do(Exprs, Exprs), // TODO: Maybe merge into one Expr list ?
    Catch(Exprs),
}

#[derive(Debug, Clone)]
pub struct Clause {
    pub pats: Vec<Pat>,
    pub when: Exprs,
    pub res: Exprs,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Var(Var),
    Lit(Lit),
    List(Vec<Pat>),
    Tuple(Vec<Pat>),
    Alias(Var, Box<Pat>),
}
