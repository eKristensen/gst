-module('fun-to-fun').

% Function calling another function to consume part of session type
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([foo/1, bar/3]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

% TODO: Bad: Syntax does not allow custom tags after function definitions! Maybe we should have used comments instead?
% Though I would have ad to use erlang directly then.
-session("'foo'(new(!integer. ?string. !string. ?integer.  end.))   ").
% Duplicating new(!integer. ?string. !string. ?integer.  end.) here is not nice.
-session("'bar'(new(!integer. ?string. !string. ?integer.  end.),consume(!string. ?integer.),_) ").

% TODO (maybe) Consider to rename consume to lin og linear

-spec foo(new()) -> integer().
foo(ServerPid) ->
    SessionID = gen_server_plus:new(ServerPid), 
    Res = gen_server_plus:call(ServerPid,SessionID,42+3),

    bar(ServerPid, SessionID, Res).

-spec bar(new(),consume(),string()) -> integer().
bar(ServerPid, SessionID, Str) ->
    gen_server_plus:call(ServerPid,SessionID,Str).
