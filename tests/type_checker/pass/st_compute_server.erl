-module(st_compute_server).
-behavior(gen_server_plus).

%% Preliminary Typing Idea
% elp:ignore W0013 (misspelled_attribute)
-mspec('[?neg !ready. ?int !int,?add !ready ?int !received ?int !int]').

% By Emil Kristensen, ITU 2023-2024

% Static server from Gradual Session Types Paper
% A session represents a channel in the code below.

% Public functions in this module
-export([start_link/0]).

-export([handle_new_session_call/3,handle_plus_call/4,handle_plus_cast/3]).

start_link() ->
    gen_server_plus:start_link(?MODULE, [], []).

-spec handle_new_session_call('neg', pid(), map()) -> tuple();
% New session neg
% Preliminary Typing Idea
% Typing: ?{SessionState,neg}.!atom
% SessionState=no_session
handle_new_session_call(neg, _From, GlobalState) ->
    io:format("DEBUG: Server got neg choice i new session~n"),
    {reply, ready, neg, GlobalState};

-spec handle_new_session_call('add', pid(), map()) -> tuple().
% New session add
% Preliminary Typing Idea
% Typing: ?{SessionState,add}.!atom
% SessionState=no_session
handle_new_session_call(add, _From, GlobalState) ->
    io:format("DEBUG: Server got add choice i new session~n"),
    {reply, ready, add, GlobalState}.

-spec(integer(), pid(), 'neg')
% Finish related session neg
% Preliminary Typing Idea
% Typing: ?{SessionState,int}.!int
% SessionState=neg
handle_plus_call(V1, _From, neg, GlobalState) ->
    io:format("DEBUG: Server got last message in neg session~n"),
    {reply,{result,-V1},[],GlobalState}; % TODO Something to close session

% Receive first value and save it in the session state
% Preliminary Typing Idea
% Typing: ?{SessionState,int}.!atom
% SessionState=add
handle_plus_call(V1, _From, add, GlobalState) ->
    io:format("DEBUG: Server got first value in add session~n"),
    {reply,received,{add,V1},GlobalState};

% Receive last value and finish computation
% Preliminary Typing Idea
% Typing: ?{SessionState,int}.!int
% SessionState={add,int}
handle_plus_call(V2, _From, {add,V1}, GlobalState) ->
    io:format("DEBUG: Server got last message in add session~n"),
    {reply,{result,V1+V2},[],GlobalState}. % TODO Something to close session

handle_plus_cast(_, SessionState, GlobalState) ->
    io:format("No cast support~n"),
    {noreply, SessionState, GlobalState}.
