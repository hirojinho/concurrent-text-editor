-module(main).
-export([start/0, init/0, consume/1, publish/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start() ->
    application:ensure_started(amqp_client),
    {ok, _} = rabbitmq:start(),
    init().

init() ->
    % Open a connection to the RabbitMQ server
    {ok, Connection} = amqp_connection:start("localhost", 5672, <<"guest">>, <<"guest">>),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Declare = #'exchange.declare'{
        exchange = <<"my_exchange">>,
        type = <<"direct">>,
        durable = true
    },
    amqp_channel:call(Channel, Declare),
    consume(Channel).

consume(Channel) ->
    Queue = <<"my_queue">>,
    amqp_channel:call(Channel, #'queue.declare'{
        queue = Queue,
        durable = true
    }),
    amqp_channel:call(Channel, #'queue.bind'{
        queue = Queue,
        exchange = <<"my_exchange">>,
        routing_key = <<"my_routing_key">>
    }),
    amqp_channel:subscribe(Channel, Queue, self()),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, Body} ->
            io:format("Received: ~s~n", [Body]),
            amqp_channel:ack(Channel, Tag),
            loop(Channel);
        _Other ->
            loop(Channel)
end.

publish(Message) ->
    {ok, Connection} = amqp_connection:start("localhost", 5672, <<"guest">>, <<"guest">>),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Publish = #'basic.publish'{
        routing_key = <<"my_routing_key">>,
        exchange = <<"my_exchange">>
    },
    amqp_channel:call(Channel, Publish, #amqp_msg{payload = Message}),
    amqp_connection:close(Connection).
