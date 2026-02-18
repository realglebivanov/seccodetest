-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting client, ~p", [application:get_all_env(client)]),
    client_sup:start_link().

stop(_State) ->
    ok.
