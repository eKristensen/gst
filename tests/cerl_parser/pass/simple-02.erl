-module('simple-02').
-export([foo/2]).

-spec foo(boolean(), number()) -> (boolean());
         (pid(), list()) -> reference().
foo(A,B) when is_boolean(A) -> B == A;
foo(A,B) -> A ! B,
            receive
              S -> S
            end.
