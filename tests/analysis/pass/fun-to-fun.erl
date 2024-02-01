-module('fun-to-fun').

% Function calling another function to consume part of session type
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([foo/1, bar/3]).

-type new() :: {}. % changed from server() as constructor
-type ongoing() :: {}. % changed from session()

% TODO: Bad: Syntax does not allow custom tags after function definitions! Maybe we should have used comments instead?
% Though I would have ad to use erlang directly then.
-session("'foo'(new(!number. ?string. !string. ?number.  end.)) -> _, [ SessionID:  end. ]  ").
% Duplicating new(!number. ?string. !string. ?number.  end.) here is not nice.
-session("'bar'(new(!number. ?string. !string. ?number.  end.),ongoing(!string. ?number. -> end.),_) -> _, []").

-spec foo(new()) -> number().
foo(ServerPid) ->
    SessionID = gen_server_plus:call(ServerPid,new), 
    Res = gen_server_plus:call(ServerPid,SessionID,42+3),

    bar(ServerPid, SessionID, Res).

-spec bar(new(),ongoing(),string()) -> number().
bar(ServerPid, SessionID, Str) ->
    gen_server_plus:call(ServerPid,SessionID,Str).
