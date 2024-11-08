-module('gradual-base-02').

% By Emil Kristensen, ITU 2024

-export([foo/1, bar/1]).

-type new() :: {}.
-type consume() :: {}.

-session("'foo'(_)").
-session("'bar'(_)").

-spec foo(dynamic()) -> integer().
foo(Value) ->
  bar(Value).

-spec bar(integer()) -> integer().
bar(Value) ->
  Value.

