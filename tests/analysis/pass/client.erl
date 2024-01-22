-module(client).

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/2]).

-type service() :: {}.

-service_def("calculator: +(neg: !int. ?int. ; add: !int. !int. ?int.)").

%      Connect calculator and ServerPID            Can return be ST or service or both?
%                        ↓↓                                       ↓↓
-session("'negation'(calculator, _) -> SessionID: &(neg: !int ?int), _  ").
% TO ADD: multi-options for session.
-spec negation(service(),number()) -> number().
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
    % TODO: There is an undetected spec violation. The return type for negation is "no_return()" but should be "number()"
