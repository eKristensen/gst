-module('gradual-01').

% By Emil Kristensen, ITU 2024

-export([foo/2]).

-type new() :: {}.
-type consume() :: {}.

-session("'foo'(new(!integer. ?integer. end),_)").

-spec foo(new(), any()) -> integer().
foo(ServerPid, Value) ->
  SessionID = gen_server_plus:new(ServerPid),
  Res = gen_server_plus:call(ServerPid, SessionID, Value),
  gen_server_plus:close(ServerPid, SessionID),
  Res.
