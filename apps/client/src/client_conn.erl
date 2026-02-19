-module(client_conn).

-behaviour(gen_server).

-export([
    child_spec/0,
    start_link/0,
    send/1,
    init/1,
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
    {linger, {true, 5000}}
]).

-include_lib("proto/include/proto.hrl").

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE, start => {client_conn, start_link, []}}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send(req()) -> resp().
send(Req) ->
    gen_server:call(?MODULE, Req).

init([]) ->
    process_flag(trap_exit, true),
    logger:info("Started a new connection ~p", [self()]),
    self() ! recv,
    {ok, PortNo} = application:get_env(client, server_port),
    {ok, Sock} = gen_tcp:connect("localhost", PortNo, ?SOCKET_OPTS),
    {ok, client_conn_state:new(Sock)}.

handle_call(Req, From, State) ->
    case client_conn_state:send(State, From, Req) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, closed} ->
            logger:info("Connection closed on send"),
            {stop, closed, State};
        {error, Reason} ->
            logger:warning("Unable to send data due to ~p", [Reason]),
            {stop, Reason, State}
    end.

handle_cast(Request, Sock) ->
    logger:warning("Unexpected cast ~p received", [Request]),
    {noreply, Sock}.

handle_info(recv, State) ->
    case client_conn_state:recv_update(State) of
        {ok, NewState} ->
            self() ! recv,
            {noreply, NewState};
        {ok, Event, NewState} ->
            self() ! recv,
            client_cli ! Event,
            {noreply, NewState};
        {ok, From, Resp, NewState} ->
            self() ! recv,
            gen_server:reply(From, Resp),
            {noreply, NewState};
        {error, closed} ->
            logger:info("Connection closed on recv"),
            {stop, closed, State};
        {error, Reason} ->
            logger:warning("Unable to receive data due to ~p", [Reason]),
            {stop, Reason, State}
    end;
handle_info(Info, State) ->
    logger:warning("Unexpected message received ~p", [Info]),
    {noreply, State}.

terminate(Reason, Sock) ->
    logger:info("Closing socket because of ~p", [Reason]),
    ok = client_tcp_sock:close(Sock),
    ok.
