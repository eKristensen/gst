-module('recursive-no-common-return').

% By Emil Kristensen, ITU 2024

-export([foo/2, bar/3]).

-type new() :: {}.
-type consume() :: {}.

% TODO: Ask Marco: Really not allow less choices in consume?
% I cannot type this module as is, without giving the whole function to the consume function.

% TODO: Fix Session type syntax, dot is not predictable and err msg is very hard to read.
-session("'foo'(new(rec T. +{send:!integer. ?integer. T, stop: end }),_)").
-session("'bar'(new(rec T. +{send:!integer. ?integer. T, stop: end }),consume(rec T. +{send: !integer. ?integer. T, stop: -}),_)").
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
    gen_server_plus:call(ServerPid, SessionID, send),
    gen_server_plus:call(ServerPid, SessionID, V1).

