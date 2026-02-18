-module(server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ok = server_auth:init(),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 1
    },
    ChildSpecs = [
        server_conn_sup:child_spec(),
        server_listener:child_spec(),
        server_cli:child_spec()
    ],
    {ok, {SupFlags, ChildSpecs}}.
