-module(proto).

-include_lib("proto/include/proto.hrl").

-export([
    seq_id/1,
    next_seq_id/0,
    encode/1,
    decode/1
]).

-spec seq_id(req() | resp()) -> pos_integer().
seq_id(#send_message{seq_id = SeqId}) -> SeqId;
seq_id(#message_sent{seq_id = SeqId}) -> SeqId;
seq_id(#auth{seq_id = SeqId}) -> SeqId;
seq_id(#auth_ok{seq_id = SeqId}) -> SeqId;
seq_id(#auth_error{seq_id = SeqId}) -> SeqId;
seq_id(#already_connected{seq_id = SeqId}) -> SeqId.

-spec next_seq_id() -> pos_integer().
next_seq_id() ->
    erlang:unique_integer([monotonic, positive]).

-spec encode(event() | req() | resp()) -> binary().
encode(#message{author = Author, text = Text}) ->
    json:encode(#{type => <<"message">>, author => Author, text => Text});
encode(#send_message{seq_id = SeqId, text = Text}) ->
    json:encode(#{seq_id => SeqId, type => <<"send_message">>, text => Text});
encode(#message_sent{seq_id = SeqId}) ->
    json:encode(#{seq_id => SeqId, type => <<"message_sent">>});
encode(#auth{seq_id = SeqId, login = Login, password = Password}) ->
    json:encode(#{seq_id => SeqId, type => <<"auth">>, login => Login, password => Password});
encode(#auth_ok{seq_id = SeqId}) ->
    json:encode(#{seq_id => SeqId, type => <<"auth_ok">>});
encode(#auth_error{seq_id = SeqId}) ->
    json:encode(#{seq_id => SeqId, type => <<"auth_error">>});
encode(#already_connected{seq_id = SeqId}) ->
    json:encode(#{seq_id => SeqId, type => <<"already_connected">>}).

-spec decode(binary()) -> {ok, proto_entity()} | error.
decode(Bin) ->
    try json:decode(Bin) of
        Data -> from_data(Data)
    catch
        _:_Reason -> error
    end.

-spec from_data(json:decode_value()) -> {ok, proto_entity()} | error.
from_data(#{
    <<"type">> := <<"message">>,
    <<"author">> := Author,
    <<"text">> := Text
}) when
    is_binary(Author) and is_binary(Text)
->
    {ok, #message{author = Author, text = Text}};
from_data(#{
    <<"seq_id">> := SeqId,
    <<"type">> := <<"send_message">>,
    <<"text">> := Text
}) when is_binary(Text) ->
    {ok, #send_message{seq_id = SeqId, text = Text}};
from_data(#{<<"seq_id">> := SeqId, <<"type">> := <<"message_sent">>}) ->
    {ok, #message_sent{seq_id = SeqId}};
from_data(#{
    <<"seq_id">> := SeqId,
    <<"type">> := <<"auth">>,
    <<"login">> := Login,
    <<"password">> := Password
}) when
    is_binary(Login) and is_binary(Password)
->
    {ok, #auth{seq_id = SeqId, login = Login, password = Password}};
from_data(#{<<"seq_id">> := SeqId, <<"type">> := <<"auth_ok">>}) ->
    {ok, #auth_ok{seq_id = SeqId}};
from_data(#{<<"seq_id">> := SeqId, <<"type">> := <<"auth_error">>}) ->
    {ok, #auth_error{seq_id = SeqId}};
from_data(#{<<"seq_id">> := SeqId, <<"type">> := <<"already_connected">>}) ->
    {ok, #already_connected{seq_id = SeqId}};
from_data(_Data) ->
    error.
