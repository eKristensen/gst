-module(client).

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/2, addition/3]).

-type new() :: {}. % changed from server() as constructor
-type consume() :: {}. % changed from session()

% 
%      Connect calculator and ServerPID            Can return be ST or service or both?
%                        ↓↓                                       ↓↓
-session("'negation'(new(+{neg: !integer. ?integer. end, add: !integer. ?'received'. !integer. ?integer. end}),_)").

% 
%      Connect calculator and ServerPID            Can return be ST or service or both?
%                        ↓↓                                       ↓↓
-session("'addition'(new(+{neg: !integer. ?integer. end, add: !integer. ?'received'. !integer. ?integer. end}),_,_)").
% TODO: ADD: multi-options for session.
-spec negation(new(),integer()) -> integer().
negation(ServerPid,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    SessionID = gen_server_plus:new(ServerPid), 
    io:format("DEBUG: Client started session~n"),
    _ = gen_server_plus:call(ServerPid,SessionID,neg), % TODO: Also accepts !atom. !integer. ?integer. end. (session type without a choice)
    io:format("DEBUG: Client chose neg~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Client sent integer and got response: ~w~n", [Res]),
    gen_server_plus:close(ServerPid, SessionID),
    Res.

-spec addition(new(),integer(),integer()) -> integer().
addition(ServerPid,V1, V2) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    SessionID = gen_server_plus:new(ServerPid), 
    io:format("DEBUG: Client started session~n"),
    _ = gen_server_plus:call(ServerPid,SessionID,add), % TODO: Also accepts !atom. !integer. ?integer. end. (session type without a choice)
    io:format("DEBUG: Client chose add~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    _ = gen_server_plus:call(ServerPid,SessionID,V1),
    Res = gen_server_plus:call(ServerPid,SessionID,V2),
    io:format("Client sent two integers and got response: ~w~n", [Res]),
    gen_server_plus:close(ServerPid, SessionID),
    Res.
