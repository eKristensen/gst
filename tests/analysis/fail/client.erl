-module(client).

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/2]).

-type fresh() :: {}.
-type ongoing() :: {}.

%                       That name does not matter, what matters is that it matches the function argument
% Client scripts         ↓↓
-session ("'negation'(fresh(!number. ?number. !string.),_)").
-spec negation(fresh(),number()) -> number().
negation(ServerPid,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    {SessionID,ready} = gen_server_plus:call(ServerPid,new,neg),
    io:format("DEBUG: Client got ready~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Got response: ~w~n", [Res]).
