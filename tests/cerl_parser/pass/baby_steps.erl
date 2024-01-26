-module(baby_steps).
-export([computeServer/0, negationClient/2, additionClient/3]).

% By Emil Kristensen, ITU 2023

% File content: A very simple proof of concept for sending messages
% What is Erlang? How does it work?
% Example based on gradual session typing paper.

% Pid represents a "channel" for a functions mailbox.

computeServer() ->
    % Should the function check is_integer/1 ?
    % Or is it better to let erlang crash?
    receive
        {From, neg, V1} ->
            From ! -V1;
        {From, add, V1, V2} ->
            From ! V1+V2
    % after 2000 -> timeout
    % Without restart server only respond once.
    % restarting is not trivial / raceconditions on Pid might happen
    % https://learnyousomeerlang.com/errors-and-processes#naming-processes
    % Example: start_critic2()
    end.

% select neg implied
negationClient(ServerPid,V1) ->
    ServerPid ! {self(), neg, V1},
    receive
        Y -> Y
    after 2000 -> timeout
    % Since computeServer does not restart it is closed automatically
    end.

additionClient(ServerPid,V1,V2) ->
    ServerPid ! {self(), add, V1, V2},
    receive
        Y -> Y
    after 2000 -> timeout
    end.
