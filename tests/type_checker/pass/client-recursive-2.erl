-module('client-recursive').

% By Emil Kristensen, ITU 2024

-export([foo/2, bar/3]).

-type new() :: {}.
-type consume() :: {}.

% Note: What Marco meant by "end" in consume was to stop doing things.
% TODO: Fix somehow with main session type. Right now it should fail because not fully consumed
% But fails in call contract typing

% TODO: Replace "stop(T.)" with "stop(end.)" in ServerPid session type. And try to type it.

% TODO: Fix Session type syntax, dot is not predictable and err msg is very hard to read.
-session("'foo'(new(rec T. +{send(!integer. ?integer. T.), stop(T.), realstop(end.)}..),_)").
-session("'bar'(new(rec T. +{send(!integer. ?integer. T.), stop(T.)}..),consume(rec T. +{send(!integer. ?integer. T.), stop(.)}..),_)").
%         &{send(!integer. ?integer. &{send(!integer. ?integer.)}.)}.),_)").


-spec foo(new(),integer()) -> integer().
foo(ServerPid,V1) ->
    SessionID = gen_server_plus:new(ServerPid),
    Res = bar(ServerPid,SessionID, V1),
    _ = gen_server_plus:call(ServerPid, SessionID, stop),
    Res.

-spec bar(new(),consume(),integer()) -> integer().
bar(ServerPid, SessionID, V1) ->
    _ = gen_server_plus:call(ServerPid, SessionID, send),
    _ = gen_server_plus:call(ServerPid, SessionID, V1),
    % gen_server_plus:call(ServerPid, SessionID, stop),
    42.
%    gen_server_plus:call(ServerPid, SessionID, V1).

