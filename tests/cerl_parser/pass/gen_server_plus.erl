-module(gen_server_plus).
-behavior(gen_server).

% By Emil Kristensen, ITU 2024

% Version 1.1

% IMPORTANT: This code intentionally introduces memory leaks.
% Proof of Concept code only! NOT READY FOR PRODUCTION!

% The intention of this code is to extend gen_server with references
% that can be used as session identifiers by the client to relate messages

% Goal: Take care of more than ch_server.erl is doing, lessen the burden of
% implementing some kind of session identifier.

% Approach: Implement as needed, do not try to be complete yet.

% TODO / Open issue: How to implement return types from handle call/cast in a efficient way?
% A result is that handle_plus_call/cast does not support the full set of return calls
% See: https://github.com/erlang/otp/blob/c6b97c2abb51adfbf3658c78a07001ab99f1fcc4/lib/stdlib/src/gen_server.erl#L1143

% Simple export for now, only allow starting, and not much else really. Not OTP Compliant as is,
% but this is more like PoC towards OTP compliance if PoC even seems to be useful.
-export([start_link/3]).

% GenServerPlus API
-export([call/3,call/4]).

% GenServer API Callbacks.
% TODO: Consider allow setting the initial global state and initial session state for new sessions.
% Toy example: Global state count total number of requests.
-export([init/1, handle_call/3, handle_cast/2]).

% From gen_server.erl
-type from() ::	{Client :: pid(), Tag :: reply_tag()}.
-opaque reply_tag() :: gen:reply_tag().

% FYI: The intention is that callback is a module name, global is a term(), session is a map from session id to terms
% No in depth optimization or deep thought has gone into these types. Maybe a map for sessions isn't the right long-term choice.
-record(plus_state, {callback, global, sessions}).

-callback handle_plus_call(Request :: term(), From :: from(),
                      SessionState :: term(), GlobalState :: term()) ->
    {reply, Reply :: term(), NewSessionState :: term(), NewGlobalState :: term()} |
    {reply, Reply :: term(), NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term()} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewSessionState :: term(), NewGlobalState :: term()} |
    {stop, Reason :: term(), NewSessionState :: term(), NewGlobalState :: term()}.
-callback handle_new_session_call(Request :: term(), From :: from(),
                      SessionState :: term(), GlobalState :: term()) ->
    {reply, Reply :: term(), NewSessionState :: term(), NewGlobalState :: term()} |
    {reply, Reply :: term(), NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term()} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewSessionState :: term(), NewGlobalState :: term()} |
    {stop, Reason :: term(), NewSessionState :: term(), NewGlobalState :: term()}.
-callback handle_plus_cast(Request :: term(), SessionState :: term(), GlobalState :: term()) ->
    {noreply, NewSessionState :: term(), NewGlobalState :: term()} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewSessionState :: term(), NewGlobalState :: term()}.
-callback handle_new_session_cast(Request :: term(), SessionState :: term(), GlobalState :: term()) ->
    {noreply, NewSessionState :: term(), NewGlobalState :: term()} |
    {noreply, NewSessionState :: term(), NewGlobalState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewSessionState :: term(), NewGlobalState :: term()}.

% gen_server_plus delegates the server parts to gen_server
% However gen_server_plus has it's own callback so it must remember those
% TODO: Support init arguments. None for now for simplicity
start_link(Mod, _Args, _Options) -> % Args and options not used right now.
    % store Mod in state
    gen_server:start_link(?MODULE, Mod, []).

% "Hack": Uses Args for the CallbackMod name.
init(Mod) ->
    {ok,#plus_state{callback=Mod, sessions=maps:new()}}.

% Handle call which are expected to be related to a session
handle_call({'$gen_plus_call',SessionID,Payload},From,State) when is_reference(SessionID) ->
    io:format("DEBUG: GenPlus. Session Call~n"),
    % lookup session
    case maps:find(SessionID,State#plus_state.sessions) of
        {ok, SessionState} ->
            % Relies on make_ref from gen-server for the direct response
            {Action, Msg, UpdatedSessionState, UpdatedGlobalState} = 
                (State#plus_state.callback):handle_plus_call(Payload,From,SessionState,State#plus_state.global);
        _NoSession ->
            % Learn new session. Default state for new session is "new_session"
            {Action, Msg, UpdatedSessionState, UpdatedGlobalState} = 
            (State#plus_state.callback):handle_new_session_call(Payload,From,State#plus_state.global) % Does it even make sense to pass msg here?
    end,
    % Save updated session
    UpdatedSessions = maps:put(SessionID,UpdatedSessionState,State#plus_state.sessions),
    % Update global state record that gen_server remembers and let gen_server handle the rest
    {Action, Msg, State#plus_state{global=UpdatedGlobalState, sessions=UpdatedSessions}};

% All other calls are accepted and passed to callback module
handle_call(Payload,From,State) ->
    io:format("DEBUG: GenPlus. Other Call~n"),
    {Action, Msg, _, UpdatedGlobalState} = 
        (State#plus_state.callback):handle_plus_call(Payload,From,other,State#plus_state.global),
    {Action, Msg, State#plus_state{global=UpdatedGlobalState}}.

% TODO: Handle state in cast, ignored for now to see if call even works.
handle_cast({'$gen_plus_call',SessionID,Payload},State) ->
    io:format("DEBUG: GenPlus. Session Cast~n"),
    io:format("TODO: GenPlus handle_cast: Implementation not finished!~n"),
    % lookup session
    case maps:find(SessionID,State#plus_state.sessions) of
        {ok, SessionState} ->
            % output of handle_plus_cast handle.... hmm...
            % Maybe it is ok as it is for call, but with cast the session id needs to be added again. but. cast does not have any reply so simple solution!
            (State#plus_state.callback):handle_plus_cast(Payload,SessionState,State#plus_state.global);
        _NoSession -> % TODO: Maybe better to just learn new session? Required otherwise not communication is possible.
                      % when is_reference(SessionID) ->
                      % add_new_session function?
            (State#plus_state.callback):handle_new_session_cast(Payload,State#plus_state.global) % Does it even make sense to pass msg here?
    end;

% All other cast are accepted and passed to callback module
handle_cast(Payload,State) ->
    io:format("DEBUG: GenPlus. Other Cast~n"),
    io:format("TODO: GenPlus handle_cast: Implementation not finished!~n"),
    (State#plus_state.callback):handle_plus_cast(Payload,other,State#plus_state.global).


% Without explicit timeout. Wrap gen_server adding SessionID to the request.
% The recipient should use gen_server_plus as well to parse the incoming message.
call(ServerPid,new,Request) when is_pid(ServerPid) ->
    FreshSessionID = make_ref(),
    {FreshSessionID,
        gen_server:call(ServerPid,{'$gen_plus_call',FreshSessionID,Request})};

call(ServerPid,SessionID,Request) when is_pid(ServerPid), is_reference(SessionID) ->
    {SessionID,
        gen_server:call(ServerPid,{'$gen_plus_call',SessionID,Request})}.

% With explicit timeout
% TODO: Not possible to create new session with timeout yet!
call(ServerPid,SessionID,Request,Timeout) ->
    gen_server:call(ServerPid,{'$gen_plus_call',SessionID,Request},Timeout).

