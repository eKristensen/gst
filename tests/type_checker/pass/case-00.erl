-module('case-00').
-export([foo/1]).

-session("'foo'(_)").
-spec foo(boolean()) -> boolean().
  foo(A) -> case A of
                true -> false;
                false -> true
                        end.
