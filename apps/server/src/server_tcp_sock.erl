-module(server_tcp_sock).

-export([maybe_send/2, close/1]).
-export_type([send_ret/0, error/0]).

-type send_ret() :: ok | error().
-type error() :: {error, closed | timeout | inet:posix()}.

-spec maybe_send(gen_tcp:socket(), binary()) -> send_ret().
maybe_send(Sock, Bin) ->
    case gen_tcp:send(Sock, Bin) of
        ok -> ok;
        {error, {timeout, Rest}} -> try_to_resend(Sock, Rest);
        {error, closed} -> {error, closed}
    end.

-spec try_to_resend(gen_tcp:socket(), binary()) -> send_ret().
try_to_resend(Sock, Data) ->
    maybe
        ok ?= inet:setopts(Sock, [{packet, raw}]),
        ok ?= try_to_resend(Sock, Data, 5),
        ok ?= inet:setopts(Sock, [{packet, 2}])
    end.

-spec try_to_resend(gen_tcp:socket(), binary(), non_neg_integer()) -> send_ret().
try_to_resend(Sock, Data, Retries) ->
    case gen_tcp:send(Sock, Data) of
        ok -> ok;
        {error, {timeout, _Rest}} when Retries == 0 -> {error, timeout};
        {error, {timeout, Rest}} -> try_to_resend(Sock, Rest, Retries - 1);
        {error, _Reason} = Error -> Error
    end.

-spec close(gen_tcp:socket()) -> ok.
close(Sock) ->
    ok = gen_tcp:shutdown(Sock, write),
    ok = drain(Sock),
    ok = gen_tcp:close(Sock),
    ok.

-spec drain(gen_tcp:socket()) -> ok.
drain(Sock) ->
    case gen_tcp:recv(Sock, 0, 0) of
        {ok, _Packet} -> drain(Sock);
        {error, timeout} -> ok;
        {error, closed} -> ok;
        {error, Reason} -> logger:warning("Error encoutered while draining ~p", [Reason])
    end.
