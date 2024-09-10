-module('fun-to-fun').

% Function calling another function to consume part of session type
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([foo/1, bar/3]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

% TODO: Bad: Syntax does not allow custom tags after function definitions! Maybe we should have used comments instead?
% Though I would have ad to use erlang directly then.
-session("'foo'(new(?integer. !integer. ?integer. !string.  end.))   ").
% Duplicating new(!integer. ?string. !string. ?integer.  end.) here is not nice.
-session("'bar'(new(?integer. !integer. ?integer. !string.  end.),consume(!integer. ?string.),_) ").

% TODO (maybe) Consider to rename consume to lin og linear

-spec foo(new()) -> string().
foo(ServerPid) ->
    SessionID = gen_server_plus:new(ServerPid), 
    Res = gen_server_plus:call(ServerPid,SessionID,42+3),

    bar(ServerPid, SessionID, Res).

-spec bar(new(),consume(),integer()) -> string().
bar(ServerPid, SessionID, Input) ->
    gen_server_plus:call(ServerPid,SessionID,Input).
