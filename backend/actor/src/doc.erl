-module(doc).

-export([start/1]).

start(InitialDoc) ->
    spawn(fun() -> loop(InitialDoc) end).

loop(CurrentDoc) ->
    receive
        {newDoc, NewDocPayload} ->
            % Check if the new document is different from the current one
            if
                NewDocPayload =/= CurrentDoc ->
                    io:format("Updating document from: ~p to: ~p~n", [CurrentDoc, NewDocPayload]),
                    loop(NewDocPayload);
                true ->
                    io:format("Document remains the same: ~p~n", [CurrentDoc]),
                    loop(NewDocPayload)
            end;
        terminate ->
            io:format("Terminating the process with last state: ~p~n", [CurrentDoc]),
            ok; % Optional cleanup if necessary
        Other ->
            io:format("Received unexpected message: ~p. Current state: ~p~n", [Other, CurrentDoc]),
            loop(CurrentDoc)
    end.