-module(st_client).

% By Emil Kristensen, ITU 2023

% Client for gst servers both dyn and static. Interface is the same.

% Public functions in this module
-export([negation/2,addition/3]).

% Client scripts
% Preliminary Typing Idea: +{neg:!atom.?atom.!int.?int.end}
negation(ServerPid,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    {SessionID,ready} = gen_server_plus:call(ServerPid,new,neg),
    io:format("DEBUG: Client got ready~n"),
    % Ask for computation with SessionID returned from previous call
    {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Got response: ~w~n", [Res]).

% Preliminary Typing Idea: +{add:!atom.?atom.!int.?atom.!int.?int.end}
addition(ServerPid,V1,V2) ->
    io:format("DEBUG: Started add~n"),
    % Send first message and get SessionID
    {SessionID,ready} = gen_server_plus:call(ServerPid,new,add),
    io:format("DEBUG: Client got ready~n"),
    % Ask for computation with SessionID returned from previous call
    {SessionID,received} = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("DEBUG: Server received first value~n"),
    {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V2),
    io:format("Got response: ~w~n", [Res]).
