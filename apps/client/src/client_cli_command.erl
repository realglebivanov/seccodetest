-module(client_cli_command).

-export([parse/1, run/2]).

-include_lib("proto/include/proto.hrl").

-spec parse(binary()) -> {ok, req()} | {error, invalid_args | no_input}.
parse(<<>>) ->
    {error, no_input};
parse(<<"/auth", Rest/binary>>) ->
    case binary:split(Rest, <<" ">>, [global, trim_all]) of
        [Login, Password] ->
            SeqId = proto:next_seq_id(),
            {ok, #auth{seq_id = SeqId, login = Login, password = Password}};
        _ ->
            {error, invalid_args}
    end;
parse(Data) ->
    {ok, #send_message{seq_id = proto:next_seq_id(), text = Data}}.

-spec run(port(), event() | req()) -> true.
run(Port, Message = #message{}) ->
    Author = Message#message.author,
    Text = Message#message.text,
    port_command(Port, <<Author/binary, ": ", Text/binary, "\n">>);
run(Port, Auth = #auth{}) ->
    case client_conn:send(Auth) of
        #auth_ok{} -> port_command(Port, <<"Auth: ok\n">>);
        #auth_error{} -> port_command(Port, <<"Auth: invalid credentials\n">>);
        #already_connected{} -> port_command(Port, <<"Auth: already connected\n">>)
    end;
run(Port, SendMessage = #send_message{text = Text}) ->
    case client_conn:send(SendMessage) of
        #message_sent{} ->
            port_command(Port, <<"Me: ", Text/binary, "\n">>);
        #auth_error{} ->
            port_command(Port, <<"Auth: unauthorized\n">>)
    end.
