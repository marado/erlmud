-module(world).
-compile([export_all]).


% world_process(State) is the main function for the world process which stores the state of the world 
% and responds to queries and commands from the users
world_start() ->
    ets:new(users, [ordered_set, named_table]),
    ets:new(locations, [ordered_set, named_table]),
    ets:new(directions, [ordered_set, named_table]),
    make_location(plaza, #{ n => agora, s => mercado, w => academia, e => salida_sur }),
    make_location(agora, #{ s => plaza, e => templo, n => acropolis }),
    make_location(templo, #{ w => agora, n => acropolis }),
    make_location(mercado, #{ n => plaza, w => academia }),
    make_location(academia, #{ e => plaza, s => mercado }),
    make_location(salida_sur, #{ w => plaza }),
    make_location(acropolis, #{ s => agora }),
    world_process().

world_process() ->
    receive
        {try_login, Pid, User} -> 
            CanonicUser = canonical_username(User),
            case ets:member(users, CanonicUser) of
                false ->
                    true = ets:insert_new(users, {CanonicUser, #{ location => plaza }}),
                    change_user_location(CanonicUser, plaza),
                    Pid ! { login_succeded, User };
                true ->
                    Pid ! { login_failed, User }
            end,
            world_process();
        {location, Pid, User} ->
            CanonicalUser = canonical_username(User),
            #{ location := Location } = ets:lookup_element(users, CanonicalUser, 2),
            Users = ets:lookup_element(locations, Location, 2),
            Directions = ets:lookup_element(directions, Location, 2),
            Pid ! {location, Location, #{users => Users, directions => Directions}},
            world_process()
    end.

canonical_username(UserString) ->
    UserString2 = lists:flatten(string:tokens(UserString, " ")),
    UserString3 = lists:flatten(string:tokens(UserString2, "-")),
    UserString4 = lists:flatten(string:tokens(UserString3, "_")),
    UserString5 = lists:flatten(string:tokens(UserString4, ".")),
    string:lowercase(UserString5).

make_location(Name, Directions) ->
    ets:insert(locations, {Name, []}),
    ets:insert(directions, {Name, Directions}).
directions_from(Location) ->
    {_, Dirs} = ets:lookup(directions, Location),
    Dirs.
users_in(Location) ->
    UsersIn = ets:lookup(locations, Location),
    UsersIn.

change_user_location(CanonicUser, NewLocation) ->
    UserInfo = ets:lookup_element(users, CanonicUser, 2),
    #{ location := OldLocation } = UserInfo,
    NewUserInfo = UserInfo#{ location := NewLocation },
    ets:insert(users, {CanonicUser, NewUserInfo}),
    OldLocationInfo = ets:lookup_element(locations, OldLocation, 2),
    UpdatedOldLocationInfo = lists:delete(CanonicUser, OldLocationInfo),
    ets:insert(locations, {OldLocation, UpdatedOldLocationInfo}),
    NewLocationInfo = ets:lookup_element(locations, NewLocation, 2),
    UpdatedNewLocationInfo = [ CanonicUser | NewLocationInfo],
    ets:insert(locations, {NewLocation, UpdatedNewLocationInfo}).
    
place_description(Location) ->
    case Location of
        plaza ->
            ""
    end.
