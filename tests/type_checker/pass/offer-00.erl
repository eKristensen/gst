-module('offer-00').
-export([foo/1]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

-session("'foo'(consume(+{tiger: end, lion: end }))").
-spec foo(consume()) -> boolean().
  foo(A) -> case A of
                'tiger' -> false;
                'lion' -> true
                        end.
