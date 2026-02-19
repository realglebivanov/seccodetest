-module(server_tcp_sock).

-export([send/2, close/1]).
-export_type([socket_ret/0, error/0]).

-type socket_ret() :: ok | error().
-type error() :: {error, closed | timeout | inet:posix()}.

-spec send(gen_tcp:socket(), binary()) -> socket_ret().
send(Sock, Bin) ->
    case gen_tcp:send(Sock, Bin) of
        ok -> ok;
        {error, {timeout, Rest}} -> try_to_resend(Sock, Rest);
        {error, _Reason} = Err -> Err
    end.

-spec try_to_resend(gen_tcp:socket(), binary()) -> socket_ret().
try_to_resend(Sock, Data) ->
    case inet:setopts(Sock, [{packet, raw}]) of
        ok ->
            Res = try_to_resend(Sock, Data, _Retries = 5),
            Restore = inet:setopts(Sock, [{packet, 2}]),
            case Res of
                ok -> Restore;
                {error, _Reason} = Err -> Err
            end;
        {error, _Reason} = Err ->
            Err
    end.

-spec try_to_resend(gen_tcp:socket(), binary(), non_neg_integer()) -> socket_ret().
try_to_resend(Sock, Data, Retries) ->
    case gen_tcp:send(Sock, Data) of
        ok -> ok;
        {error, {timeout, _Rest}} when Retries == 0 -> {error, timeout};
        {error, {timeout, Rest}} -> try_to_resend(Sock, Rest, Retries - 1);
        {error, _Reason} = Error -> Error
    end.

-spec close(gen_tcp:socket()) -> ok.
close(Sock) ->
    maybe
        ok ?= gen_tcp:shutdown(Sock, write),
        ok ?= drain(Sock),
        ok ?= gen_tcp:close(Sock),
        ok
    else
        {error, Reason} ->
            logger:warning("Error encoutered while closing: ~p", [Reason])
    end.

-spec drain(gen_tcp:socket()) -> socket_ret().
drain(Sock) ->
    case gen_tcp:recv(Sock, 0, 1) of
        {ok, _Packet} -> drain(Sock);
        {error, timeout} -> ok;
        {error, closed} -> ok;
        {error, _Reason} = Err -> Err
    end.
