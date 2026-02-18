-record(auth, {login, password}).
-record(send_message, {text}).

-record(message, {author, text}).
-record(auth_error, {}).
-record(already_connected, {}).

-type event() ::
    #auth{}
    | #send_message{}
    | #message{}
    | #auth_error{}
    | #already_connected{}.
