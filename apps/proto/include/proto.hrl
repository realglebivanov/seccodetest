-record(message, {author, text}).

-record(auth, {seq_id, login, password}).
-record(send_message, {seq_id, text}).

-record(auth_ok, {seq_id}).
-record(auth_error, {seq_id}).
-record(already_connected, {seq_id}).
-record(message_sent, {seq_id}).

-type proto_entity() :: event() | req() | resp().

-type event() :: #message{}.
-type req() :: #auth{} | #send_message{}.
-type resp() ::
    #message_sent{}
    | #auth_ok{}
    | #auth_error{}
    | #already_connected{}.
