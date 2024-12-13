-module('simple-02').
-export([foo/2]).

-session("'foo'(_,_);(_,_)").
-spec foo(boolean(), integer()) -> (boolean());
         (pid(), list()) -> reference().
foo(A,B) when is_boolean(A) -> B == A;
foo(A,B) -> A ! B,
            receive
              S -> S
            end.
