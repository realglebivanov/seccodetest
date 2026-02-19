-module(client_cli).

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

-include_lib("proto/include/proto.hrl").

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{id => ?MODULE, start => {client_cli, start_link, []}}.

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
    case client_cli_command:parse(Data) of
        {ok, Command} -> client_cli_command:run(Port, Command);
        {error, no_input} -> ok;
        {error, invalid_args} -> port_command(Port, <<"Invalid arguments\n">>)
    end,
    {noreply, Port};
handle_info({Port, {data, {noeol, Data}}}, Port) ->
    port_command(Port, <<"Input too large: ~p">>, [Data]),
    {noreply, Port};
handle_info({Port, eof}, Port) ->
    port_close(Port),
    io:format("stdin closed~n"),
    {stop, normal, Port};
handle_info(Message = #message{}, Port) ->
    client_cli_command:run(Port, Message),
    {noreply, Port};
handle_info(Msg, Port) ->
    io:format("Ignoring msg : ~p~n", [Msg]),
    {noreply, Port}.

terminate(_Reason, Port) ->
    port_close(Port),
    io:format("stdin closed~n"),
    ok.
