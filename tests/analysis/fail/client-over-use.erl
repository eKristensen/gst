-module('client-over-use').

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/2]).

-type new() :: {}. % changed from server() as constructor
-type ongoing() :: {}. % changed from session()

% 
%      Connect calculator and ServerPID            Can return be ST or service or both?
%                        ↓↓                                       ↓↓
-session("'negation'(new(+{neg(!number. ?number. end.), add(!number. !number. ?number. end.)}.),_) -> _, [ SessionID:  end. ]  ").
% TO ADD: multi-options for session.
-spec negation(new(),number()) -> number().
negation(ServerPid,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    SessionID = gen_server_plus:call(ServerPid,new), 
    io:format("DEBUG: Client started session~n"),
    _ = gen_server_plus:call(ServerPid,SessionID,neg), % TODO: Also accepts !atom. !number. ?number. end. (session type without a choice)
    io:format("DEBUG: Client chose neg~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    % Check that "end." is not used
    Res = gen_server_plus:call(ServerPid,SessionID,fish), 
    io:format("Client sent number and got response: ~w~n", [Res]),
    Res.