-module(client_conn_state).

-export([new/1, recv_update/1, send/3, terminate/1]).

-record(state, {
    sock :: gen_tcp:socket(),
    reqs :: #{pos_integer() => gen_server:from()}
}).

-include_lib("proto/include/proto.hrl").

-spec new(gen_tcp:socket()) -> #state{}.
new(Sock) ->
    #state{sock = Sock, reqs = #{}}.

-spec recv_update(#state{}) ->
    {ok, #state{}}
    | {ok, event(), #state{}}
    | {ok, gen_server:from(), resp(), #state{}}
    | {error, corrupted_seq | invalid_frame | client_tcp_sock:reason()}.
recv_update(State) ->
    case gen_tcp:recv(State#state.sock, 0, 100) of
        {ok, Bin} -> decode_update(State, Bin);
        {error, timeout} -> {ok, State};
        {error, _Reason} = Err -> Err
    end.

decode_update(State, Bin) ->
    case proto:decode(Bin) of
        {ok, Msg = #message{}} ->
            logger:debug("Received an event from the server ~p", [Msg]),
            {ok, Msg, State};
        {ok, Resp} ->
            logger:debug("Received a resp from the server ~p", [Resp]),
            update(State, Resp);
        error ->
            logger:warning("Invalid frame received ~p", [Bin]),
            {error, invalid_frame}
    end.

update(State, Resp) ->
    case maps:take(proto:seq_id(Resp), State#state.reqs) of
        error -> {error, corrupted_seq};
        {From, Reqs} -> {ok, From, Resp, State#state{reqs = Reqs}}
    end.

-spec send(
    #state{},
    gen_server:from(),
    req()
) -> {ok, #state{}} | server_tcp_sock:error().
send(State, From, Req) ->
    logger:debug("Sending a req to the server ~p", [Req]),
    case client_tcp_sock:send(State#state.sock, proto:encode(Req)) of
        ok ->
            SeqId = proto:seq_id(Req),
            Reqs = State#state.reqs,
            {ok, State#state{reqs = maps:put(SeqId, From, Reqs)}};
        {error, _Reason} = Err ->
            Err
    end.

-spec terminate(#state{}) -> ok.
terminate(State) ->
    ok = client_tcp_sock:close(State#state.sock).
