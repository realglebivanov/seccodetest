-module(server_conn).

-behaviour(gen_server).

-export([
    child_spec/0,
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-include_lib("proto/include/proto.hrl").

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {server_conn, start_link, []},
        restart => temporary
    }.

-spec start_link(gen_tcp:socket()) -> gen_server:start_ret().
start_link(Sock) ->
    gen_server:start_link(?MODULE, Sock, []).

-spec stop(gen_server:server_ref()) -> ok.
stop(Ref) ->
    gen_server:call(Ref, stop).

init(Sock) ->
    process_flag(trap_exit, true),
    logger:info("Accepted a new connection ~p", [self()]),
    self() ! recv,
    {ok, server_conn_state:new(Sock)}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    logger:warning("Unexpected call ~p received from ~p", [Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    logger:warning("Unexpected cast ~p received", [Request]),
    {noreply, State}.

handle_info(#message{} = Message, State) ->
    case server_conn_state:handle(State, Message) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, closed} ->
            logger:info("Connection closed on send"),
            {stop, normal, State};
        {error, Reason} ->
            logger:warning("Unable to send data due to ~p", [Reason]),
            {stop, Reason, State}
    end;
handle_info(recv, State) ->
    case server_conn_state:recv_handle(State) of
        {ok, NewState} ->
            self() ! recv,
            {noreply, NewState};
        {error, closed} ->
            logger:info("Connection closed on recv"),
            {stop, normal, State};
        {error, Reason} ->
            logger:warning("Unable to receive data due to ~p", [Reason]),
            {stop, Reason, State}
    end;
handle_info(Info, State) ->
    logger:warning("Unexpected message received ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    logger:info("Closing socket because of ~p", [Reason]),
    ok = server_conn_state:terminate(State),
    ok.
