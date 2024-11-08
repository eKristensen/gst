-module('gradual-base-01').

% By Emil Kristensen, ITU 2024

-export([foo/1]).

-type new() :: {}.
-type consume() :: {}.

-session("'foo'(_)").

-spec foo(dynamic()) -> integer().
foo(Value) ->
  Value.
