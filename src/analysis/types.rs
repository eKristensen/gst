// https://github.com/gertab/ElixirST/blob/75d098f51df40b5ff1022c7dc56a695b0f3da9d9/lib/elixirst/session_type.ex#L122
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Types {
    Single(String),
    Tuple(Vec<Types>),
    Cons(Vec<Types>),
}
