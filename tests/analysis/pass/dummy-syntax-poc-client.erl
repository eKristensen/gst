-module('dummy-syntax-poc-client').

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/3]).

-type service() :: pid().
-type session() :: any().

% Translation between my syntax and Marco syntax
% fresh=server()    == Construct fresh session
% ongoing=session() == Ongoing session

% Service tag here can be considered optional. It works as a constant/marco.
%                                              Add end to be explicit.
%                                                 ↓↓
% -service("calculator: +{neg(!int. ?int. !string. end.), add(!int. !int. ?int. end.)}").

%      Connect calculator and ServerPID                                  Direct return       Side-effect/binders
%                        ↓↓                                                  ↓↓                   ↓↓
% -session("'negation'(service(calculator), session(&(neg: !int ?int)), _) -> !string, [ SessionID: !string. end. , ... ]  ").

% Counting arguments is bad, but got no better solution now. ideally it would be something like
% -session("'negation/2: ServerPid=service(calculator)").

% TO ADD: multi-options for session.
-spec negation(service(),session(),number()) -> number().
negation(ServerPid,SessionId0,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    SessionID = gen_server_plus:call(ServerPid,new),
    io:format("DEBUG: Client started session~n"),
    {} = gen_server_plus:call(ServerPid,SessionID,neg),
    io:format("DEBUG: Client chose neg~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Client sent number and got response: ~w~n", [Res]),
    {SessionID}.
    % TODO: There is an undetected spec violation. The return type for negation is "no_return()" but should be "number()"
