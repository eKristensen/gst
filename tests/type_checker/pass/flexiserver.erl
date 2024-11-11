-module('flexiserver').

% By Emil Kristensen, ITU 2024
% elp:ignore W0013 (misspelled_attribute)
-mspec("&{
        neg: <'fserve'>. ?integer. !integer. end,
        add: <'fserve'>. ?integer. !'received'. <'fserve'>. ?integer. !integer. end
       }").
% TODO: Maybe usi

-type new() :: {}.
-type consume() :: {}.

-session("'start_link'()").
-session("'handle_plus_cast'(_,_,_)").


% Public functions in this module
-export([start_link/0]).

-export([handle_plus_call/5,handle_plus_cast/3]).

-spec start_link() -> any().
start_link() ->
    gen_server_plus:start_link(?MODULE, [], []).

% Note: TODO: Write assumptions/quicks: 'received'= Response will be sent but not as part of session type, more like a "confirmation". 'session_end' instructs gen server plus to destory the session.

-spec handle_plus_call('neg'    , pid(), 'start' , {}       , map()) -> {'reply'  , 'received', 'fserve' , dynamic(), map()};
                      ('add'    , pid(), 'start' , {}       , map()) -> {'reply'  , 'received', 'fserve' , dynamic(), map()};
                      (integer(), pid(), 'fserve', dynamic(), map()) -> {'reply'  , dynamic() , dynamic(), dynamic(), map()};
                      (any()    , any(), any()   , any()    , any()) -> {'noreply', any()     , any()}.
%% flexiServer / "dynServer"
handle_plus_call(neg, _From, start, {}, GlobalState) ->
    {reply, received, fserve, {1,fun(X) -> -X end}, GlobalState};
handle_plus_call(add, _From, start, {}, GlobalState) ->
    {reply, received, fserve, {2,fun(X) -> fun(Y) -> X+Y end end}, GlobalState};

%% fserve / "serveOp"
handle_plus_call(Value, _From, fserve, {Arity,Op}, GlobalState) ->
    if Arity == 0 ->
        {reply, Op, session_end, {}, GlobalState};
    true ->
        {reply, received, fserve, {Arity-1, apply(Op, Value)}, GlobalState}
    end;

% Catch all
handle_plus_call(_, _, _, SessionState, GlobalState) ->
  io:format("Unknown plus call ignored. If only supported calls are made this message should never appear.~n"),
  {noreply, SessionState, GlobalState}.

-spec handle_plus_cast(any(), any(), any()) -> {'noreply', any(), any()}.
handle_plus_cast(_, SessionState, GlobalState) ->
    io:format("No cast support~n"),
    {noreply, SessionState, GlobalState}.
