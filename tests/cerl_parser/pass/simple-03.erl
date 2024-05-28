-module('simple-03').
-export([foo/3]).

-spec foo(boolean(), number(), float()) -> (boolean());
         (pid(), list(), reference()) -> reference().
foo(A,B,C) when is_boolean(A), C > 1 -> B == A;
foo(A,B,C) -> A ! B,
            receive
              S -> S
            end,
            C.
