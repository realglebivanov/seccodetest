-module(server_conn_state).

-export([new/1, recv_handle/1, handle/2, terminate/1]).

-record(state, {sock, status}).
-record(connected, {}).
-record(authed, {login}).

-include_lib("proto/include/proto.hrl").

-spec new(gen_tcp:socket()) -> #state{}.
new(Sock) ->
    #state{sock = Sock, status = #connected{}}.

-spec recv_handle(#state{}) -> {ok, #state{}} | server_tcp_sock:error().
recv_handle(State = #state{}) ->
    case gen_tcp:recv(State#state.sock, 0, 100) of
        {ok, Bin} -> decode_handle(State, Bin);
        {error, timeout} -> {ok, State};
        {error, _Reason} = Err -> Err
    end.

-spec decode_handle(#state{}, binary()) -> {ok, #state{}} | server_tcp_sock:error().
decode_handle(State, Bin) ->
    case proto:decode(Bin) of
        {ok, Msg} ->
            handle(State, Msg);
        error ->
            logger:warning("Invalid frame received ~p", [Bin]),
            {ok, State}
    end.

-spec handle(#state{}, event()) -> {ok, #state{}} | server_tcp_sock:error().
handle(State = #state{status = #connected{}}, Msg = #auth{}) ->
    case server_auth:login(Msg#auth.login, Msg#auth.password) of
        ok -> {ok, #state{sock = State#state.sock, status = #authed{login = Msg#auth.login}}};
        {error, invalid_credentials} -> maybe_send(State, #auth_error{});
        {error, already_connected} -> maybe_send(State, #already_connected{})
    end;
handle(State = #state{status = Status = #authed{}}, Msg = #send_message{}) ->
    Message = #message{
        author = Status#authed.login,
        text = Msg#send_message.text
    },
    server_conn_sup:broadcast(Message),
    {ok, State};
handle(State = #state{status = #authed{}}, Msg = #message{}) ->
    maybe_send(State, Msg);
handle(State, Msg) ->
    logger:warning("Received invalid msg ~p in state ~p", [Msg, State]),
    {ok, State}.

-spec terminate(#state{}) -> ok.
terminate(State = #state{status = #connected{}}) ->
    ok = server_tcp_sock:close(State#state.sock);
terminate(State = #state{status = Status = #authed{}}) ->
    ok = server_tcp_sock:close(State#state.sock),
    ok = server_auth:logout(Status#authed.login).

-spec maybe_send(#state{}, event()) -> {ok, #state{}} | server_tcp_sock:error().
maybe_send(State, Message) ->
    case server_tcp_sock:maybe_send(State#state.sock, proto:encode(Message)) of
        ok -> {ok, State};
        {error, _Reason} = Err -> Err
    end.
