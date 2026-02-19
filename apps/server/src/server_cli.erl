-module(server_cli).

-behaviour(gen_server).

-export([
    init/1,
    child_spec/0,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-type command() ::
    {useradd, server_auth:login(), server_auth:password()}
    | {userdel, server_auth:login()}.

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE, start => {server_cli, start_link, []}}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({fd, 0, 1}, [binary, eof, stream, {line, 128}]),
    {ok, Port}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, {eol, Data}}}, Port) ->
    case parse(Data) of
        {ok, Command} -> run(Port, Command);
        {error, no_input} -> ok;
        {error, invalid_args} -> port_command(Port, <<"Invalid arguments\n">>);
        {error, invalid_command} -> port_command(Port, <<"Invalid command\n">>)
    end,
    {noreply, Port};
handle_info({Port, {data, {noeol, Data}}}, Port) ->
    port_command(Port, <<"Input too large: ~p">>, [Data]),
    {noreply, Port};
handle_info({Port, eof}, Port) ->
    port_close(Port),
    io:format("stdin closed~n"),
    {stop, normal, Port};
handle_info(Msg, Port) ->
    io:format("Ignoring msg : ~p~n", [Msg]),
    {noreply, Port}.

terminate(_Reason, Port) ->
    port_close(Port),
    io:format("stdin closed~n"),
    ok.

-spec parse(binary()) ->
    {ok, command()} | {error, invalid_args | invalid_command | no_input}.
parse(Data) ->
    case binary:split(Data, <<" ">>, [global, trim_all]) of
        [<<"/useradd">>, Login, Password] -> {ok, {useradd, Login, Password}};
        [<<"/useradd">> | _] -> {error, invalid_args};
        [<<"/userdel">>, Login] -> {ok, {userdel, Login}};
        [<<"/userdel">> | _] -> {error, invalid_args};
        [] -> {error, no_input};
        _ -> {error, invalid_command}
    end.

-spec run(port(), command()) -> true.
run(Port, {useradd, Login, Password}) ->
    case server_auth:add(Login, Password) of
        ok -> port_command(Port, <<"User added\n">>);
        error -> port_command(Port, <<"Failed to add a user\n">>)
    end;
run(Port, {userdel, Login}) ->
    case server_auth:del(Login) of
        ok -> port_command(Port, <<"User deleted\n">>);
        error -> port_command(Port, <<"Failed to delete a user\n">>)
    end.
