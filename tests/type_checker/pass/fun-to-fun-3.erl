-module('fun-to-fun-3').

% Function calling another function to consume part of session type
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([foo/1, bar/3]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

% TODO: Bad: Syntax does not allow custom tags after function definitions! Maybe we should have used comments instead?
% Though I would have ad to use erlang directly then.
-session("'foo'(new(!integer. ?string. !integer. ?string.  end))   ").
-session("'bar'(new(!integer. ?string. !integer. ?string.  end),consume(!integer. ?string. -),_) ").

-spec foo(new()) -> string().
foo(ServerPid) ->
    SessionID = gen_server_plus:new(ServerPid), 
    bar(ServerPid, SessionID, 42),
    Res = bar(ServerPid, SessionID, 43),
    gen_server_plus:close(ServerPid, SessionID),
    Res.

-spec bar(new(),consume(),integer()) -> string().
bar(ServerPid, SessionID, Input) ->
    gen_server_plus:call(ServerPid,SessionID,Input).
