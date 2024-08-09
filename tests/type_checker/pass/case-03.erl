-module('case-03').
-export([foo/1]).

-session("'foo'(_); (_)").

-spec foo(boolean()) -> boolean();
         (integer()) -> integer().
foo(A) when is_boolean(A) -> case A of
                true -> false;
                false -> true
                        end;
foo(B) -> B.
