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

% start() initiates system processes for the MUD Server and starts the listen socket
start() ->
    io:format("**** Multi User Dungeon Server Started ****~n"),
    io:format("** Starting sessions manager process **~n"),
    SessionsProcess = spawn(?MODULE, sessions_process, [[]]),
    WorldState = #{ users => #{} },
    WorldProcess = spawn(?MODULE, world_process, [WorldState]),
    SystemProcesses = #{ sessions => SessionsProcess, world => WorldProcess },
    case gen_tcp:listen(0, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSock} ->
            {ok, Port} = inet:port(ListenSock),
            io:format("conectado a ~p~n",[Port]),
            server(ListenSock, SystemProcesses);
        {error, Reason} -> 
            {error, Reason}
    end.

% world_process(State) is the main function for the world process which stores the state of the world 
% and responds to queries and commands from the users
world_process(State) ->
    receive
        {new_user, User} -> 
            #{users := Users} = State,
            world_process(State#{ users := Users#{User => #{ location => plaza }}});
        {location, Pid, User} ->
            #{users := #{ User := #{ location := Location }}} = State,
            Pid ! {location, Location},
            world_process(State)
    end.

% server(ListenSock, SystemProcesses) multiplexes accept connections into individual sessions
server(ListenSock, SystemProcesses = #{ sessions := SessionsProcess }) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            Conn = #{ socket => Socket, system_processes => SystemProcesses },
            Pid = spawn(?MODULE, session, [Conn]),
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

% session(Conn) starts the user session, tries to login then transfer control to succesive functions/states of the user session in the MUD
session(Conn = #{ socket := Socket, system_processes := #{ sessions := SessionsProcess, world := WorldPid }}) ->
    case login(Conn) of
        {ok, NewConn = #{ username := Name }} ->
            io:format("~s ha entrado al Server~n", [Name]),
            WorldPid ! {new_user, Name},
            user_logged(NewConn);
        _ ->
            io:format("Error login~n"),
            exit(vaya)
    end.

% login(Conn) ask for a name to the user
login(Conn = #{ socket := Socket }) ->
    message(Conn, "BIENVENIDO AL GRAN MUD\nTu nombre?: "),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            User = string:trim(binary_to_list(Data)),
            {ok, Conn#{ username => User }};
        _ -> error
    end.

% utility function to have io:format capabilities into string
sformat(FS, Args) ->
    lists:flatten(io_lib:format(FS, Args)).

% next session state after login
user_logged(Conn = #{ username := Name }) ->
    message(Conn, sformat("Bienvenido ~s, te recuerdo...~n", [Name])),
    user_entry(Conn).

user_entry(Conn) ->
    describe_location(Conn),
    user_loop(Conn).

% the user loop
user_loop(Conn) ->
    case ask_action(Conn) of
        exit ->
            io:format("Finalizado user_loop~n");
        UpdatedConn -> 
            user_loop(UpdatedConn)
    end.

ask_action(Conn = #{ socket := Socket, system_processes := #{ sessions := SessionsProcess } }) ->
    message(Conn, "> "),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            perform_action(Conn, binary:bin_to_list(string:trim(Data)));
        {tcp_closed, Socket} ->
            SessionsProcess ! {quit, self()},
            exit;
        _ -> io:format("Mala cosa ~n"),
             exit
    end.

% describe a location, triggered usually when the character enters a new zone
describe_location(Conn = #{ username := Name, system_processes := #{ world := WorldPid }}) ->
    WorldPid ! {location, self(), Name},
    receive
        {location, Location} ->
            message(Conn, sformat("Estas en ~s~n", [Location]))
    end.

% prints a message to the user
message(#{ socket := Socket }, Message) ->
    gen_tcp:send(Socket, Message).

% catalog of actions to the user
perform_action(Conn, Data) ->
    Commands = string:tokens(Data, " "),
    Cmd = hd(Commands),
    if
        Cmd == "say"; Cmd == "s" ->
            io:format("Say command");
        true ->
            message(Conn, "No entiendo lo que quieres decir\n")
    end,
    Conn.
