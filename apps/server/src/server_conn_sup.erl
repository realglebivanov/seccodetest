-module(server_conn_sup).

-behaviour(supervisor).

-export([child_spec/0, start_link/0, start_child/1, init/1, broadcast/1]).

-include_lib("proto/include/proto.hrl").

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE, start => {?MODULE, start_link, []}, type => supervisor}.

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(gen_tcp:socket()) -> ok | supervisor:startchild_err().
start_child(Sock) ->
    case supervisor:start_child(?MODULE, [Sock]) of
        {ok, _Child} -> ok;
        {ok, _Child, _Info} -> ok;
        {error, _Reason} = Err -> Err
    end.

-spec broadcast(#message{}) -> ok.
broadcast(Msg) ->
    lists:foreach(
        fun
            (Pid) when Pid =/= self() -> Pid ! Msg;
            (_Self) -> ok
        end,
        server_auth:get_active_pids()
    ).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [server_conn:child_spec()],
    {ok, {SupFlags, ChildSpecs}}.
