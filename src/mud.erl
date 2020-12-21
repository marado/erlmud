-module(mud).
-compile(export_all).
-import(world, [world_start/0]).
-import(user, [session/1]).

-define(RED,   "\x1B[31m").
-define(GRN,   "\x1B[32m").
-define(YEL,   "\x1B[33m").
-define(BLU,   "\x1B[34m").
-define(MAG,   "\x1B[35m").
-define(CYN,   "\x1B[36m").
-define(WHT,   "\x1B[37m").
-define(RESET, "\x1B[0m").

% start() initiates system processes for the MUD Server and starts the listen socket
start() ->
    io:format("**** Multi User Dungeon Server Started ****~n"),
    io:format("** Starting sessions manager process **~n"),
    SessionsProcess = spawn(?MODULE, sessions_process, [[]]),
    WorldProcess = spawn(world, world_start, []),
    SystemProcesses = #{ sessions => SessionsProcess, world => WorldProcess },
    case gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSock} ->
            {ok, Port} = inet:port(ListenSock),
            io:format("conectado a ~p~n",[Port]),
            server(ListenSock, SystemProcesses);
        {error, Reason} -> 
            {error, Reason}
    end.

% server(ListenSock, SystemProcesses) multiplexes accept connections into individual sessions
server(ListenSock, SystemProcesses = #{ sessions := SessionsProcess }) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            Conn = #{ socket => Socket, system_processes => SystemProcesses },
            Pid = spawn(user, session, [Conn]),
            ok = gen_tcp:controlling_process(Socket, Pid),
            SessionsProcess ! {new, Pid},
            server(ListenSock, SystemProcesses);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

% sessions_process(Sessions) mantains a list of active sessions (as processes)
% when there is a succesful login in the system, the session notifies this process
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

