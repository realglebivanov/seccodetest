-module(proto).

-include_lib("proto/include/proto.hrl").

-export([encode/1, decode/1]).

-spec encode(event()) -> binary().
encode(#message{author = Author, text = Text}) ->
    json:encode(#{type => "message", author => Author, text => Text});
encode(#send_message{text = Text}) ->
    json:encode(#{type => "send_message", text => Text});
encode(#auth{login = Login, password = Password}) ->
    json:encode(#{type => "auth", login => Login, password => Password});
encode(#auth_error{}) ->
    json:encode(#{type => "auth_error"});
encode(#already_connected{}) ->
    json:encode(#{type => "already_connected"}).

-spec decode(binary()) -> {ok, event()} | error.
decode(Bin) ->
    try json:decode(Bin) of
        Data -> from_data(Data)
    catch
        _:_Reason -> error
    end.

-spec from_data(json:decode_value()) -> {ok, event()} | error.
from_data(#{
    <<"type">> := <<"message">>,
    <<"author">> := Author,
    <<"text">> := Text
}) when
    is_binary(Author) and is_binary(Text)
->
    {ok, #message{author = Author, text = Text}};
from_data(#{
    <<"type">> := <<"send_message">>,
    <<"text">> := Text
}) when is_binary(Text) ->
    {ok, #send_message{text = Text}};
from_data(#{
    <<"type">> := <<"auth">>,
    <<"login">> := Login,
    <<"password">> := Password
}) when
    is_binary(Login) and is_binary(Password)
->
    {ok, #auth{login = Login, password = Password}};
from_data(#{<<"type">> := <<"auth_error">>}) ->
    {ok, #auth_error{}};
from_data(#{<<"type">> := <<"already_connected">>}) ->
    {ok, #already_connected{}};
from_data(_Data) ->
    error.
