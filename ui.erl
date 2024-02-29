-module(ui).
-export([
    print_info/1,
    print_message/1,
    print_messages/1
]).

-include("records.hrl").

print_info(Info) ->
    case Info of
        login_fail_another_user_logged_in ->
            io:format("Login Fail: Another user already logged in.~n");
        login_aborted_max_clients ->
            io:format("Login Aborted: Max clients limit reached at server.~n");
        login_aborted_same_name_exist ->
            io:format("Login Aborted: User with same name already exists.~n");
        not_logged_in ->
            io:format("You are not logged in, please login first.~n");
        logged_out ->
            io:format("You are logged out.~n");
        {user_not_logged_in_private_message_saved, ToName}->
            io:format("~s is not logged in, but the private message is stored.~n", [ToName]);
        {connected_clients, ConnectedClients} ->
            io:format("Currently Connected Clients: ~p~n", [ConnectedClients]);
        {all_clients, AllClients} ->
            io:format("Clients: ~p~n", [AllClients]);
        not_admin ->
            io:format("You are not an Admin.~n");
        {no_user, Who} ->
            io:format("No user with username ~s exits.~n", [Who]);
        {already_admin, Who} ->
            io:format("~s is already an admin.~n", [Who]);
        {admins, Admins} ->
            io:format("Admins: ~p~n", [Admins]);
        {current_chat_topic, Topic, set_by, SetBy} ->
            io:format("Current chat topic is ~s set by ~s.~n", [Topic, SetBy]);
        chat_topic_update_fail_same_topic ->
            io:format("Topic Updation Fail: Choose a different topic than current topic.~n");
        {not_logged_in, Who} ->
            io:format("~s is not logged in.~n", [Who]);
        kicked_out ->
            io:format("You are kicked out.~n");
        you_are_muted ->
            io:format("You are muted.~n");
        {you_are_muted_till, Time} ->
            io:format("You are muted till ~s.~n", [time_formatter(Time)]);
        you_are_unmuted ->
            io:format("You are unmuted.~n");
        max_clients_update_aborted ->
            io:format("Maximum Clients Update Aborted: Already logged in users are greater than count.~n");
        done ->
            ok
    end.

gen_notif(Notif) ->
    case Notif of
        {enter_user, {new, Who}} ->
            formatter("~s has entered the room", [Who]);
        {enter_user, {back, Who}} ->
            formatter("~s has entered back in the room", [Who]);
        {exit_user, Who} ->
            formatter("~s has left the room", [Who]);
        {admin_promotion, Who, By} ->
            formatter("~s is promoted to admin by ~s", [Who, By]);
        {topic_change, Who, What} ->
            formatter("~s changed the chat topic to ~s", [Who, What]);
        {user_muted, Who, By} ->
            formatter("~s is muted by ~s", [Who, By]);
        {user_muted, Who, By, till, Time} ->
            formatter("~s is muted by ~s till ~s", [Who, By, time_formatter(Time)]);
        {user_unmuted, Who, By} ->
            formatter("~s is unmuted by ~s", [Who, By]);
        {kick_out_user, Who, By} ->
            formatter("~s kicked out ~s", [By, Who]);
        {current_chat_topic, Topic, set_by, UserName} ->
            formatter("Current chat topic is ~s set by ~s", [Topic, UserName])
    end.

formatter(String, Args) -> lists:flatten(io_lib:format(String, Args)).

time_formatter({{Y, M, D}, {H, Min, S}}) -> formatter("~p/~p/~p ~p:~p:~p", [D, M, Y, H, Min, S]).

print_message(#erchat_message{from = From, to = To, content = Content, time = Time, type = MsgType}) ->
    Msg = case MsgType of
        notification -> formatter("*~s*", [gen_notif(Content)]);
        private -> formatter("[Private] ~s: ~s", [From, Content]);
        private_with_for -> formatter("[Private to ~s] ~s: ~s", [To, From, Content]);
        _ -> formatter("~s: ~s", [From, Content])
    end,
    io:format(formatter("[~s] ~s~n", [time_formatter(Time), Msg])).
    
print_messages(Msgs) -> 
    [print_message(Msg) || Msg <- Msgs].