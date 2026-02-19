-module(client_tcp_sock).

-export([send/2, close/1]).
-export_type([send_ret/0, error/0, reason/0]).

-type send_ret() :: ok | error().
-type reason() :: closed | timeout | inet:posix().
-type error() :: {error, reason()}.

-spec send(gen_tcp:socket(), binary()) -> send_ret().
send(Sock, Bin) ->
    case gen_tcp:send(Sock, Bin) of
        ok -> ok;
        {error, {timeout, Rest}} -> try_to_resend(Sock, Rest);
        {error, _Reason} = Err -> Err
    end.

-spec try_to_resend(gen_tcp:socket(), binary()) -> send_ret().
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
    maybe
        ok ?= gen_tcp:shutdown(Sock, write),
        ok ?= drain(Sock),
        ok ?= gen_tcp:close(Sock),
        ok
    else
        {error, Reason} ->
            logger:warning("Error encoutered while closing: ~p", [Reason])
    end.

-spec drain(gen_tcp:socket()) -> send_ret().
drain(Sock) ->
    case gen_tcp:recv(Sock, 0, 1) of
        {ok, _Packet} -> drain(Sock);
        {error, timeout} -> ok;
        {error, closed} -> ok;
        {error, _Reason} = Err -> Err
    end.
