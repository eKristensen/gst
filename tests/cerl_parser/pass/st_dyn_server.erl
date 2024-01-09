-module(st_dyn_server).
-behavior(gen_server_plus).

% By Emil Kristensen, ITU 2023

% Dynamic server from Gradual Session Types Paper
% A session represents a channel in the code below.

% Public functions in this module
-export([start_link/0]).

-export([handle_plus_call/4,handle_plus_cast/3]).

start_link() ->
    gen_server_plus:start_link(?MODULE, [], []).


% New session neg
handle_plus_call(neg, _From, no_session, GlobalState) ->
    io:format("DEBUG: Server got neg choice i new session~n"),
    {reply, ready, [{1,fun(X) -> -X end}], GlobalState};

% New session add
handle_plus_call(add, _From, no_session, GlobalState) ->
    io:format("DEBUG: Server got add choice i new session~n"),
    {reply, ready, [{2,fun(X,Y) -> X+Y end}], GlobalState};

% "serveOp" function, works on established sessions/channels
handle_plus_call(Val, _From, [{Arity,Fun} | Args], GlobalState) when is_function(Fun),is_number(Arity),Arity > 0 ->
    io:format("DEBUG: Server got more on related session~n"),
    UpdatedArgs = Args ++ [Val],
    if Arity == 1 ->
        io:format("DEBUG: got all can compute~n"),
        Result = apply(Fun,UpdatedArgs),
        {reply,{result,Result},[],GlobalState};
    true ->
        io:format("DEBUG: Accepted input. More still needed~n"),
        {reply, received,[{Arity-1,Fun} | UpdatedArgs],GlobalState}
    end.

handle_plus_cast(_, SessionState, GlobalState) ->
    io:format("No cast support~n"),
    {noreply, SessionState, GlobalState}.
