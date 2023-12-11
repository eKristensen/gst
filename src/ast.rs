

#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub exports: Vec<FunHead>,
    pub attributes: Vec<Attribute>,
    pub body: Vec<FunDef>
}

#[derive(Debug, Clone)]
pub struct FunHead {
    pub name: Fname,
    pub arity: Integer // TODO: Hmm Integer here does allow arity to be negative.....
}

#[derive(Debug, Clone)]
pub struct Fname(pub Atom);

#[derive(Debug)]
pub struct Attribute {
    pub name: Atom,
    pub value: Const
}

#[derive(Debug, Clone)]
pub struct Atom(pub String);

#[derive(Debug, Clone)]
pub struct Integer(pub i64);

#[derive(Debug, Clone)]
pub struct FunDef {
    pub head: FunHead,
    pub args: Vec<Var>,
    pub body: Expr
}

#[derive(Debug)]
pub enum Const {
    Lit(Lit),
    List(Vec<Const>),
    Tuple(Vec<Const>)
}

#[derive(Debug, Clone)]
pub struct Var(String);

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Fname(FunHead),
    Lit(Lit),
    Fun(Box<FunDef>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Let(Vec<Var>,Vec<Expr>,Vec<Expr>),
    Case(Vec<Expr>,Vec<Clause>),
    LetRec(Vec<FunDef>,Vec<Expr>),
    Apply(Vec<Expr>,Vec<Expr>),
    Call(Vec<Expr>,Vec<Expr>,Vec<Expr>),
    PrimOp(Atom,Vec<Expr>),
    Receive(Vec<Clause>,Vec<Expr>,Vec<Expr>),
    // TODO: Try
    Do(Vec<Expr>),
    Catch(Vec<Expr>)
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(Integer),
    Float(f32),
    Atom(Atom),
    Char(char),
    String(String),
    EmptyList // TODO: Rename to nil
}

#[derive(Debug, Clone)]
pub struct Clause {
    pats:Vec<Pat>,
    when: Vec<Expr>,
    res: Vec<Expr>
}

#[derive(Debug, Clone)]
pub enum Pat {
    Var(Var),
    Lit(Lit),
    List(Vec<Pat>),
    Tuple(Vec<Pat>),
    Alias(Var,Box<Pat>)
}
