-module(st_compute_server).
-behavior(gen_server_plus).

%% Preliminary Typing Idea
% elp:ignore W0013 (misspelled_attribute)
-mspec("&{
        neg: <'sel_neg'>. ?integer. !integer. end,
        add: <'sel_add'>. ?integer. !'received'. <'add_1'>. ?integer. !integer. end
       }").
% TODO: Maybe using [] for state labels is a bad idea. Same symbol as used with lists...

% TODO: Required because static type checker require full annotation. Make it work without
-session("'start_link'()").
-session("'handle_plus_call'(_,_,_,_,_);(_,_,_,_,_);(_,_,_,_,_);(_,_,_,_,_);(_,_,_,_,_);(_,_,_,_,_)").
-session("'handle_plus_cast'(_,_,_)").

% By Emil Kristensen, ITU 2023-2024

% Static server from Gradual Session Types Paper
% A session represents a channel in the code below.

% Public functions in this module
-export([start_link/0]).

-export([handle_plus_call/5,handle_plus_cast/3]).

-spec start_link() -> any().
start_link() ->
    gen_server_plus:start_link(?MODULE, [], []).

% Note: Some kind of client side ordering makes sense to avoid need for "received" msg to sync data with call.

% Note: TODO: Write assumptions/quicks: 'received'= Response will be sent but not as part of session type, more like a "confirmation". 'session_end' instructs gen server plus to destory the session.

-spec handle_plus_call('neg'    , pid(), 'start'  , {}       , map()) -> {'reply'  , 'received', 'sel_neg'    , {}       , map()};
                      ('add'    , pid(), 'start'  , {}       , map()) -> {'reply'  , 'received', 'sel_add'    , {}       , map()};
                      (integer(), pid(), 'sel_neg', {}       , map()) -> {'reply'  , integer() , 'session_end', {}       , map()};
                      (integer(), pid(), 'sel_add', {}       , map()) -> {'reply'  , 'received', 'add_1'      , integer(), map()};
                      (integer(), pid(), 'add_1'  , integer(), map()) -> {'reply'  , integer() , 'session_end', {}       , map()};
                      (any()    , any(), any()    , any()    , any()) -> {'noreply', any()     , any()}.
handle_plus_call(neg, _From, start, {}, GlobalState) ->
    {reply, received, sel_neg, {}, GlobalState};

handle_plus_call(add, _From, start, {}, GlobalState) ->
    {reply, received, sel_add, {}, GlobalState};

handle_plus_call(V1, _From, sel_neg, {}, GlobalState) ->
    {reply, -V1, session_end, {}, GlobalState};

handle_plus_call(V1, _From, sel_add, {}, GlobalState) ->
    {reply, received, add_1,V1, GlobalState};

handle_plus_call(V2, _From, add_1, V1, GlobalState) ->
    {reply, V1+V2, session_end, {}, GlobalState};

% Catch all
handle_plus_call(_, _, _, SessionState, GlobalState) ->
  io:format("Unknown plus call ignored. If only supported calls are made this message should never appear.~n"),
  {noreply, SessionState, GlobalState}.

-spec handle_plus_cast(any(), any(), any()) -> {'noreply', any(), any()}.
handle_plus_cast(_, SessionState, GlobalState) ->
    io:format("No cast support~n"),
    {noreply, SessionState, GlobalState}.
