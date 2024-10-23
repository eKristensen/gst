-module('client-not-closed').

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/2]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

-session("'negation'(new( !integer. ?integer. end),_)").
-spec negation(new(),integer()) -> integer().
negation(ServerPid,V1) ->
    SessionID = gen_server_plus:new(ServerPid), 
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Client sent integer and got response: ~w~n", [Res]),
    % Should fail because there is no close to end the session here.
    % gen_server_plus:close(ServerPid, SessionID),
    Res.

