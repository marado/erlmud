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
    case gen_tcp:listen(5679, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSock} ->
            server(ListenSock, SessionsProcess);
        {error, Reason} -> 
            {error, Reason}
    end.
    
server(ListenSock, SessionsProcess) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            io:format("Conexión aceptada~n"),
            Pid = spawn(?MODULE, session, [Socket, SessionsProcess]),
            ok = gen_tcp:controlling_process(Socket, Pid),
            SessionsProcess ! {new, Pid},
            server(ListenSock, SessionsProcess);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

sessions_process(Sessions) ->
    io:format("Current sessions: ~p~n",[Sessions]),
    receive
        {new, P} ->
            io:format("New session started ~p~n", [P]),
            sessions_process([P | Sessions]);
        {quit, P} -> 
            io:format("Session ~p ended~n", [P]),
            sessions_process(lists:delete(P, Sessions))
    end.

session(Socket, SessionsProcess) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("recibí datos~n"),
            session(Socket, SessionsProcess);
        {tcp_closed, Socket} ->
            io:format("me cerraron la movida~n"),
            SessionsProcess ! {quit, self()},
            ok;
        Any -> io:format("Mala cosa ~n")
    end.
