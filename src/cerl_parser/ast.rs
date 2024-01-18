// TODO: Refactor to be more like the core erlang paper suggests the core erlang ast should look. Parsing is more important for now though.

// "New" types
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Fname(pub Atom);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Integer(pub i64);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Var(pub String);

// AST
#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub exports: Vec<FunHead>,
    pub attributes: Vec<Attribute>,
    pub body: Vec<(FunHead, FunDef)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunHead {
    pub name: Fname,
    pub arity: Integer, // TODO: Hmm Integer here does allow arity to be negative.....
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Atom,
    pub value: Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDef {
    pub args: Vec<Var>,
    pub body: Exprs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(Integer),
    Float(f32),
    Atom(Atom),
    Char(String),      // TODO: Wut? A char is not a char, It could be e.g. $\101
    Cons(Vec<Const>),  // TODO: Implement parser
    Tuple(Vec<Const>), // TODO: Implement parser
    String(String),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Lit(Lit),
    Cons(Vec<Const>),
    Tuple(Vec<Const>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exprs {
    Single(Box<Expr>),
    Values(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MapPairType {
    Assoc,
    Exact,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapPair {
    pub pair_type: MapPairType,
    pub key: Expr,
    pub value: Expr,
}

// TODO: Maybe it is a bad idea to use Vec<Expr> instead of having Exprs
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Var),
    Fname(FunHead),
    Lit(Lit),
    Fun(FunDef),
    Cons(Vec<Exprs>),
    Tuple(Vec<Exprs>),
    Let(Vec<Var>, Exprs, Exprs),
    Case(Exprs, Vec<Clause>),
    LetRec(Vec<(FunHead, FunDef)>, Exprs),
    Apply(Exprs, Vec<Exprs>),
    Call(Exprs, Exprs, Vec<Exprs>),
    PrimOp(Atom, Vec<Exprs>),
    Receive(Vec<Clause>, Exprs, Exprs),
    Try(Exprs, Vec<Var>, Exprs, Vec<Var>, Exprs),
    Do(Exprs, Exprs), // TODO: Maybe merge into one Expr list ?
    Catch(Exprs),
    Map(Vec<MapPair>, Option<Box<Expr>>), // TODO: More transparent way to allow "update" from map or variable that contains a map
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    pub pats: Vec<Pat>,
    pub when: Exprs,
    pub res: Exprs,
}

// TODO: Can this be changed so case and let use same format for assignments???
// This structure makes code reuse much harder than it should be!
#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Var(Var),
    Lit(Lit),
    Cons(Vec<Pat>),
    Tuple(Vec<Pat>),
    Alias(Var, Box<Pat>),
}
