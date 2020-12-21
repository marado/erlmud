-module(user).
-compile([export_all]).

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

sformat_direction_option({Direction, Location}) ->
    sformat("~p -> ~p",[Direction, Location]).

% describe a location, triggered usually when the character enters a new zone
describe_location(Conn = #{ username := Name, system_processes := #{ world := WorldPid }}) ->
    WorldPid ! {location, self(), Name},
    receive
        {location, Location, LocationInfo = #{users := Users, directions := Directions}} ->
            message(Conn, sformat("Estas en ~s~n", [Location])),
            message(Conn, sformat("En este lugar están ~p~n", [Users])),
            message(Conn, "Puedes ir a los siguientes lugares:\n"),
            SS = lists:map(fun(X)->sformat_direction_option(X)end, maps:to_list(Directions)),
            message(Conn, string:join(SS, ", ") ++ ".\n")
    end.

% prints a message to the user
message(#{ socket := Socket }, Message) ->
    gen_tcp:send(Socket, Message).

% catalog of actions to the user
perform_action(Conn = #{ system_processes := #{ world := WorldPid } }, Data) ->
    Commands = string:tokens(Data, " "),
    Cmd = hd(Commands),
    if
        Cmd == "say"; Cmd == "s" ->
            io:format("Say command");
        true ->
            message(Conn, "No entiendo lo que quieres decir\n")
    end,
    Conn.
