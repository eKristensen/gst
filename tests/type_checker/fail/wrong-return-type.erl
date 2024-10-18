-module('wrong-return-type').

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Public functions in this module
-export([negation/3]).

-type new() :: pid().
-type consume() :: any().

% Translation between my syntax and Marco syntax
% fresh=server()    == Construct fresh session
% ongoing=consume() == Ongoing session

% new tag here can be considered optional. It works as a constant/marco.
%                                              Add end to be explicit.
%                                                 ↓↓
% -new("calculator: +{neg(!int. ?int. !string. end.), add(!int. !int. ?int. end.)}").

%      Connect calculator and ServerPID                                  Direct return       Side-effect/binders
%                        ↓↓                                                  ↓↓                   ↓↓
% -consume("'negation'(new(calculator), consume(&(neg: !int ?int)), _) -> !string, [ SessionID: !string. end. , ... ]  ").

% Counting arguments is bad, but got no better solution now. ideally it would be something like
% -consume("'negation/2: ServerPid=new(calculator)").

% TO ADD: multi-options for session.
-session("'negation'(new(+{neg:!integer. ?integer. end}),consume(end),_)").
-spec negation(new(),consume(),integer()) -> integer().
negation(ServerPid,SessionId0,V1) ->
    io:format("DEBUG: Started neg~n"),
    % Send first message with function and arity and get SessionID
    SessionID = gen_server_plus:new(ServerPid),
    io:format("DEBUG: Client started session~n"),
    gen_server_plus:call(ServerPid,SessionID,neg),
    io:format("DEBUG: Client chose neg~n"),
    % Ask for computation with SessionID returned from previous call

    % Changed to remove returned session id to avoid implicit when check that becomes explicit in core erlang translation Old is:
    % {SessionID,{result,Res}} = gen_server_plus:call(ServerPid,SessionID,V1),
    % Also updated return value to be just Res instead of {result,Res}
    % Updated is:
    Res = gen_server_plus:call(ServerPid,SessionID,V1),
    io:format("Client sent integer and got response: ~w~n", [Res]),
    % Fail due to wrong return type.
    SessionID.
