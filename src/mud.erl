-module(mud).
-compile(export_all).

-define(RED,   "\x1B[31m").
-define(GRN,   "\x1B[32m").
-define(YEL,   "\x1B[33m").
-define(BLU,   "\x1B[34m").
-define(MAG,   "\x1B[35m").
-define(CYN,   "\x1B[36m").
-define(WHT,   "\x1B[37m").
-define(RESET, "\x1B[0m").

start() ->
    io:format("**** Multi User Dungeon Server Started ****~n"),
    io:format("** Starting sessions manager process **~n"),
    SessionsProcess = spawn(?MODULE, sessions_process, [[]]),
    case gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSock} ->
            {ok, Port} = inet:port(ListenSock),
            io:format("conectado a ~p~n",[Port]),
            server(ListenSock, SessionsProcess);
        {error, Reason} -> 
            {error, Reason}
    end.
    
server(ListenSock, SessionsProcess) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            Pid = spawn(?MODULE, session, [Socket, SessionsProcess]),
            ok = gen_tcp:controlling_process(Socket, Pid),
            SessionsProcess ! {new, Pid},
            server(ListenSock, SessionsProcess);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

sessions_process(Sessions) ->
    io:format("Active sessions: ~p~n",[Sessions]),
    receive
        {new, P} ->
            io:format("New session started ~p~n", [P]),
            sessions_process([P | Sessions]);
        {quit, P} -> 
            io:format("Session ~p ended~n", [P]),
            sessions_process(lists:delete(P, Sessions))
    end.

% Un cliente se conecta
session(Socket, SessionsProcess) ->
    case login(Socket) of
        {ok, Name} ->
            io:format("~s ha entrado al Server~n", [Name]),
            user_logged(Socket, Name, SessionsProcess);
        _ ->
            io:format("Error login~n"),
            exit(vaya)
    end.
login(Socket) ->
    gen_tcp:send(Socket, "BIENVENIDO AL GRAN MUD\nTu nombre?: "),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            {ok, string:trim(binary_to_list(Data))};
        _ -> error
    end.

sformat(FS, Args) ->
    lists:flatten(io_lib:format(FS, Args)).

user_logged(Socket, Name, SessionsProcess) ->
    UserState = #{ name => Name,  location => plaza },
    Conn = #{ user_state => UserState, socket => Socket, sessions_process => SessionsProcess },
    message(Conn, sformat("Bienvenido ~s, te recuerdo...~n", [Name])),
    user_entry(Conn).

user_entry(Conn) ->
    describe_location(Conn),
    user_loop(Conn).

user_loop(Conn) ->
    case ask_action(Conn) of
        exit ->
            io:format("Finalizado user_loop~n");
        UpdatedConn -> 
            user_loop(UpdatedConn)
    end.

ask_action(Conn = #{ socket := Socket, sessions_process := SessionsProcess }) ->
    message(Conn, "> "),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            process_user_command(Conn, binary:bin_to_list(string:trim(Data)));
        {tcp_closed, Socket} ->
            SessionsProcess ! {quit, self()},
            exit;
        _ -> io:format("Mala cosa ~n"),
             exit
    end.

describe_location(Conn = #{ user_state := #{ location := Location }}) ->
    message(Conn, sformat("Estas en ~s~n", [Location])).

message(#{ socket := Socket }, Message) ->
    gen_tcp:send(Socket, Message).

process_user_command(Conn, Data) ->
    Commands = string:tokens(Data, " "),
    Cmd = hd(Commands),
    if
        Cmd == "say"; Cmd == "s" ->
            io:format("Say command");
        true ->
            message(Conn, "No entiendo lo que quieres decir\n")
    end,
    Conn.
