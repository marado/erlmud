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
    WorldProcess = spawn(?MODULE, world_start, []),
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
world_start() ->
    ets:new(world, [ordered_set, named_table]),
    ets:new(users, [ordered_set, named_table]),
    world_process().
canonical_username(UserString) ->
    UserString2 = lists:flatten(string:tokens(UserString, " ")),
    UserString3 = lists:flatten(string:tokens(UserString2, "-")),
    UserString4 = lists:flatten(string:tokens(UserString3, "_")),
    UserString5 = lists:flatten(string:tokens(UserString4, ".")),
    string:lowercase(UserString5).
world_process() ->
    receive
        {try_login, Pid, User} -> 
            CanonicUser = canonical_username(User),
            case ets:member(users, CanonicUser) of
                false ->
                    true = ets:insert_new(users, {CanonicUser, #{location => plaza}}),
                    Pid ! { login_succeded, User };
                true ->
                    Pid ! { login_failed, User }
            end,
            world_process();
        {location, Pid, User} ->
            #{ location := Location } = ets:lookup_element(users, canonical_username(User), 2),
            Pid ! {location, Location},
            world_process()
    end.

place_description(Location) ->
    case Location of
        plaza ->
            ""
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
    case try_login(Conn) of
        {ok, User} ->
            io:format("~s ha entrado al Server~n", [User]),
            user_logged(Conn#{username => User});
        _ ->
            message(Conn, "Saliendo...\n"),
            SessionsProcess ! {quit, self()},
            gen_tcp:close(Socket)
    end.

% try_login(Conn) ask for a name to the user
try_login(Conn = #{ socket := Socket }) ->
    message(Conn, "BIENVENIDO AL GRAN MUD\n"),
    try_login_loop(Conn, 0).

try_login_loop(Conn = #{ socket := Socket, system_processes := #{ world := WorldPid } }, Attempt) when Attempt < 3 ->
    message(Conn, sformat("[~p]Tu nombre?: ",[Attempt])),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            User = string:trim(binary_to_list(Data)),
            case try_login_name(User, WorldPid) of
                {ok, _} ->
                    {ok, User};
                {error, _} ->
                    message(Conn, "Nombre cogido\n"),
                    try_login_loop(Conn, Attempt + 1)
            end;
        _ -> error
    end;
try_login_loop(Conn, _) ->
    message(Conn, "Excedido el numero de intentos de login permitido\n"),
    error.

try_login_name(User, WorldPid) ->
    WorldPid ! {try_login, self(), User},
    receive
        {login_succeded, User} ->
            {ok, User};
        {login_failed, User} ->
            {error, User}
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
