-module(server_auth).

-export([init/0, login/2, logout/1, add/2, del/1, get_active_pids/0]).
-export_type([login/0, password/0]).

-define(USERS, server_auth_users).
-define(SESSIONS, server_auth_sessions).

-type login() :: binary().
-type password() :: binary().

-spec init() -> ok.
init() ->
    ?USERS = ets:new(?USERS, [ordered_set, public, named_table]),
    ?SESSIONS = ets:new(?SESSIONS, [ordered_set, public, named_table]),
    ok.

-spec login(
    login(),
    password()
) -> ok | {error, invalid_credentials | already_connected}.
login(Login, Password) ->
    case ets:lookup(?USERS, Login) of
        [{Login, Password}] ->
            case ets:insert_new(?SESSIONS, {Login, self()}) of
                true -> ok;
                false -> {error, already_connected}
            end;
        _ ->
            {error, invalid_credentials}
    end.

-spec logout(login()) -> ok.
logout(Login) ->
    ets:match_delete(?SESSIONS, {Login, self()}),
    ok.

-spec add(login(), password()) -> ok | error.
add(Login, Password) ->
    case ets:lookup(?USERS, Login) of
        [] ->
            ets:insert(?USERS, {Login, Password}),
            ok;
        _ ->
            error
    end.

-spec del(login()) -> ok.
del(Login) ->
    ets:delete(?USERS, Login),
    case ets:lookup(?SESSIONS, Login) of
        [{Login, Pid}] -> server_conn:stop(Pid);
        [] -> ok
    end.

-spec get_active_pids() -> [pid()].
get_active_pids() ->
    ets:select(?SESSIONS, [{{'$1', '$2'}, [], ['$2']}]).
