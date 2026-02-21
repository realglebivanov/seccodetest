-record(message, {
    author :: binary(),
    text :: binary()
}).

-record(auth, {
    seq_id :: pos_integer(),
    login :: binary(),
    password :: binary()
}).
-record(send_message, {
    seq_id :: pos_integer(),
    text :: binary()
}).

-record(auth_ok, {seq_id :: pos_integer()}).
-record(auth_error, {seq_id :: pos_integer()}).
-record(already_connected, {seq_id :: pos_integer()}).
-record(message_sent, {seq_id :: pos_integer()}).

-type proto_entity() :: event() | req() | resp().

-type event() :: #message{}.
-type req() :: #auth{} | #send_message{}.
-type resp() ::
    #message_sent{}
    | #auth_ok{}
    | #auth_error{}
    | #already_connected{}.
