-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(StartType, StartArgs) ->
    logger:info("Starting server: ~p ~p", [StartType, StartArgs]),
    server_sup:start_link().

stop(_State) ->
    ok.
