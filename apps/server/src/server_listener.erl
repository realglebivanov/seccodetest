-module(server_listener).

-behaviour(gen_server).

-export([
    child_spec/0,
    init/1,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SOCKET_OPTS, [
    {inet_backend, socket},
    {active, false},
    binary,
    {packet, 2},
    {keepalive, true},
    {linger, {true, 5000}},
    {send_timeout, 5000},
    {reuseaddr, true}
]).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE, start => {?MODULE, start_link, []}}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Port = application:get_env(server, port, 0),
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKET_OPTS),
    {ok, LPort} = inet:port(LSock),
    logger:info("Listening on port ~p", [LPort]),
    self() ! accept,
    {ok, LSock}.

handle_call(Request, From, State) ->
    logger:warning("Unexpected call ~p received from ~p", [Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    logger:warning("Unexpected cast ~p received", [Request]),
    {noreply, State}.

handle_info(accept, LSock) ->
    case accept(LSock) of
        ok ->
            self() ! accept,
            {noreply, LSock};
        {error, closed} ->
            logger:info("Listener socket is closed, terminating"),
            {stop, normal, LSock};
        {error, Reason} ->
            logger:info("Failed to accept a connection, terminating"),
            {stop, Reason, LSock}
    end;
handle_info(Info, LSock) ->
    logger:warning("Unexpected message received ~p", [Info]),
    {noreply, LSock}.

terminate(Reason, LSock) ->
    logger:info("Closing listener socket because of ~p", [Reason]),
    ok = server_tcp_sock:close(LSock).

-spec accept(gen_tcp:socket()) -> ok | server_tcp_sock:error().
accept(LSock) ->
    case gen_tcp:accept(LSock, 100) of
        {ok, Sock} -> server_conn_sup:start_child(Sock);
        {error, timeout} -> ok;
        {error, _Reason} = Err -> Err
    end.
