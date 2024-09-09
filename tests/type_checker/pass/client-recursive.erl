-module('client-recursive').

% By Emil Kristensen, ITU 2024

-export([foo/2, bar/3]).

-type new() :: {}.
-type consume() :: {}.

-session("'foo'(new(rec T. +{send(!integer. ?integer. T.), stop(end.)}.),_)").
-session("'bar'(new(rec T. +{send(!integer. ?integer. T.), stop(end.)}.),consume(+{send(!integer. ?integer. +{send(!integer. ?integer.)})}),_)").

-spec foo(new(),integer()) -> integer().
foo(ServerPid,V1) ->
    SessionID = gen_server_plus:new(ServerPid), 
    bar(ServerPid,SessionID, V1).

-spec bar(new(),consume(),integer()) -> integer().
bar(ServerPid, SessionID, V1) ->
    _ = gen_server_plus:call(ServerPid, SessionID, send),
    _ = gen_server_plus:call(ServerPid, SessionID, V1),
    _ = gen_server_plus:call(ServerPid, SessionID, send),
    gen_server_plus:call(ServerPid, SessionID, V1).

