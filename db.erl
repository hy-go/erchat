-module(db).
-export([
    is_client_logged_in/1,
    get_user_from_name/1,
    get_connected_clients_list/0,
    get_all_clients_list/0,
    login_new_user/2,
    login_back_user/2,
    store_message/1,
    get_all_messages/0,
    get_all_users_node/0,
    get_user_from_node/1,
    logout_user/1,
    is_admin/1,
    add_new_admin/1,
    list_admins/0,
    kick_out_user/1,
    get_node/1,
    mute_user/2,
    unmute_user/1
]).

-include("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER_NODE, 'server@ggn001749.greyorange.com').

do(Q) -> 
    F = fun() -> qlc:e(Q) end,
    {atomic, Data} = mnesia:transaction(F),
    Data.

get_node(From) ->
    case From of
        {Pid, _} ->
            node(Pid);
        _ ->
            node(From)
    end.

is_client_logged_in(From) ->
    {atomic, Users} = mnesia:transaction(fun() ->
        mnesia:read(users, get_node(From))
    end),
    case Users of
        [] -> 
            false;
        [User] ->
            User#erchat_user.logged_in
    end.

get_user_from_name(Who) ->
    %% TODO: use secondary index here
    Result = do(qlc:q([User || User <- mnesia:table(users), User#erchat_user.name == Who])),
    case Result of
        [] -> false;
        [User] -> User
    end.

get_connected_clients_list() ->
    do(qlc:q([User#erchat_user.name || User <- mnesia:table(users), User#erchat_user.logged_in == true, User#erchat_user.node =/= ?SERVER_NODE])).

get_all_clients_list() ->
    do(qlc:q([User#erchat_user.name || User <- mnesia:table(users)])).

login_new_user(From, Who) ->
    NewUser = #erchat_user{
        node = get_node(From), 
        name = Who
    },
    mnesia:transaction(fun() ->
        mnesia:write(users, NewUser, write)
    end).

login_back_user(From, User) ->
    UpdatedUser = User#erchat_user{
        node = get_node(From), 
        logged_in = true
    },
    mnesia:transaction(fun() ->
        mnesia:write(users, UpdatedUser, write)
    end).

store_message(Message) ->
    mnesia:transaction(fun() ->
        mnesia:write(messages, Message, write)
    end).

get_all_messages() ->
    Messages = do(qlc:q([Msg || Msg <- mnesia:table(messages)])),
    lists:sort(fun(Msg1, Msg2) ->
        Msg1#erchat_message.msg_id < Msg2#erchat_message.msg_id
    end, Messages).

get_all_users_node() ->
    do(qlc:q([User#erchat_user.node || User <- mnesia:table(users)])).

get_user_from_node(From) ->
    {atomic, [User]} = mnesia:transaction(fun() ->
        mnesia:read(users, get_node(From))
    end),
    User.

logout_user(User) ->
    UpdatedUser = User#erchat_user{logged_in = false},
    mnesia:transaction(fun() ->
        mnesia:write(users, UpdatedUser, write)
    end).

is_admin(From) ->
    User = get_user_from_node(From),
    case User#erchat_user.user_type of
        admin -> User#erchat_user.name;
        _ -> false
    end.

add_new_admin(Who) ->
    case get_user_from_name(Who) of
        false ->
            {no_user, Who};
        User ->
            case User#erchat_user.user_type of
                admin ->
                    {already_admin, Who};
                _ ->
                    UpdatedUser = User#erchat_user{user_type = admin},
                    mnesia:transaction(fun() ->
                        mnesia:write(users, UpdatedUser, write)
                    end),
                    done
            end
    end.

list_admins() ->
    do(qlc:q([User#erchat_user.name || User <- mnesia:table(users), User#erchat_user.user_type == admin])).

kick_out_user(Who) ->
    case get_user_from_name(Who) of
        false ->
            {no_user, Who};
        User ->
            case User#erchat_user.logged_in of
                false ->
                    {not_logged_in, Who};
                true ->
                    UpdatedUser = User#erchat_user{logged_in = false},
                    mnesia:transaction(fun() ->
                        mnesia:write(users, UpdatedUser, write)
                    end),
                    done
            end
    end.

mute_user(Who, Seconds) ->
    case get_user_from_name(Who) of
        false ->
            {no_user, Who};
        User ->
            case User#erchat_user.logged_in of
                false ->
                    {not_logged_in, Who};
                true ->
                    MuteFinishTime = case Seconds of
                        0 -> 0;
                        _ -> calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Seconds)
                    end,
                    UpdatedUser = User#erchat_user{muted = {true, MuteFinishTime}},
                    mnesia:transaction(fun() ->
                        mnesia:write(users, UpdatedUser, write)
                    end),
                    {done, MuteFinishTime}
            end
    end.

unmute_user(Who) ->
    case get_user_from_name(Who) of
        false ->
            {no_user, Who};
        User ->
            case User#erchat_user.logged_in of
                false ->
                    {not_logged_in, Who};
                true ->
                    UpdatedUser = User#erchat_user{muted = {false, 0}},
                    mnesia:transaction(fun() ->
                        mnesia:write(users, UpdatedUser, write)
                    end),
                    done
            end
    end.