-module(receiver).
-export([start/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

start() ->
    spawn(?MODULE, connect, []).

connect() ->
    % Open a connection to the RabbitMQ server
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        host = "localhost",
        heartbeat = 30
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    
    amqp_channel:call(Channel, #'queue.declare'{
        queue = <<"Hello">>
    }),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    Method = #'basic.consume'{
        queue = <<"Hello">>,
        no_ack = true
    },
    amqp_channel:subscribe(Channel, Method, self()),
    loop(Channel).

loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            io:format(" [x] Saw basic.consume_ok~n"),
            loop(Channel);
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Received ~p~n", [Body]),
            sender:publish(Body),
            loop(Channel)
    end.
