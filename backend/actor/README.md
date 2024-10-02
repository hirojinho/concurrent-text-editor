# Actor Model with Erlang/Elixir

## Create the Erlang/Elixir Backend

    Each user will be represented as an actor (process) in Erlang/Elixir. You can either directly handle WebSocket connections in Elixir (using Phoenix Channels) or forward user actions from Go to Erlang/Elixir through APIs.
    Each actor will be responsible for managing a user’s session and the specific edits they make to the document.
    Use the GenServer module (in Elixir) to model actors, with each actor handling the state of individual users.

## Synchronize with Go Server

    If using Elixir, set up communication between Go and Erlang/Elixir backend, possibly through an API or message queue (like RabbitMQ).
    Ensure the actor system models different versions of the document and can handle conflicts or merge changes based on user inputs.