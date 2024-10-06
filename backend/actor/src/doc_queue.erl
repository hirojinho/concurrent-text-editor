-module(doc_queue).

-export([enqueue/2]).
-export([process_queue/2]).

% Enqueue a change for the document's queue (spawn a new queue if it doesn't exist )
enqueue(DocId, Change) ->
    case doc_registry:lookup(DocId) of
        {error, not_found} ->
            % The doc does not exists, create a new one
            doc:start(DocId);
        {_, QueuePid = undefined} ->
            % Queue is not active, start a new one with the change
            spawn(?MODULE, process_queue, [DocId, [Change]]),
            % Update the registry to register the new queue
            doc_registry:update_queue(DocId, QueuePid);
        {_, QueuePid} ->
            % Queue exists and is active, enqueue the change
            QueuePid ! {enqueue, Change}
    end.

% Process the queue and terminate when done
process_queue(DocId, Queue) ->
    receive
        {enqueue, Change} ->
            % Add the change to the queue
            UpdateQueue = Queue ++ [Change],
            process_queue(DocId, UpdateQueue);
        {process_next} when Queue =/= [] ->
            % Get the next change from the queue
            [NextChange | RestQueue] = Queue,
            % Lookup the document actor and send the change to it
            {ActorPid, _} = doc_registry:lookup(DocId),
            ActorPid ! {newDoc, NextChange},
            % Process the next change after current one is done
            process_queue(DocId, RestQueue);
        {process_next} ->
            % Queue is empty, terminate the process and publish the final document
            io:format("Queue for document ~p is empty, terminating~n", [DocId]),
            doc_registry:unregister_queue(DocId),
            case doc_registry:lookup(DocId) of
                {ActorPid, _} ->
                    ActorPid ! {get_doc, self()},
                    receive
                        {doc, FinalDoc} ->
                            io:format("Final document: ~p~n", [FinalDoc]),
                            sender:publish(FinalDoc),
                            ok
                    end;
                undefined ->
                    io:format("Actor for document ~p not found during finalization~n", [DocId]),
                    ok
            end;
        stop ->
            % Stop the queue process
            io:format("Stopping queue for document ~p~n", [DocId]),
            ok;
        Other ->
            % If no new change or process message, continue the loop
            process_queue(DocId, Queue)
    after 1000 -> % Timeout for idle termination
            io:format("No messages received, terminating queue for document ~p~n", [DocId]),
            doc_registry:unregister_queue(DocId),
            ok
    end.
