-module(client_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 1
    },
    ChildSpecs = [
        client_conn:child_spec(),
        client_cli:child_spec()
    ],
    {ok, {SupFlags, ChildSpecs}}.
