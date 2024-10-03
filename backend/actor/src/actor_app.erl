%%%-------------------------------------------------------------------
%% @doc actor public API
%% @end
%%%-------------------------------------------------------------------

-module(actor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    actor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
