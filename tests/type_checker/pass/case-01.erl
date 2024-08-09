-module('case-01').
-export([foo/1, bar/1]).

-session("'foo'(_)").
-session("'bar'(_)").

-spec foo(boolean()) -> boolean().
  foo(A) -> case A of
                true -> false;
                false -> true
                        end.

-spec bar(boolean()) -> boolean().
  bar(A) -> case A of
                true -> false;
                false -> true
                        end.
