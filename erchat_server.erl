-module(erchat_server).
-behaviour(gen_server).

-export([
    start/1,
    update_max_clients/1,
    update_max_clients_update_conflict_config/1,
    print_full_message_history/0,
    update_prev_messages_count_on_login/1,
    get_connected_clients/0,
    get_all_clients/0,
    get_chat_topic/0,
    change_chat_topic/1,
    add_admin/1,
    get_admins/0,
    kick_out/1,
    mute/1,
    mute/2,
    unmute/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(db, [
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

-import(ui, [
    print_info/1
]).

-include("records.hrl").

-define(CLIENT_NAME, erchat_client).

start(Configs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Configs, []).

update_max_clients(Count) -> 
    gen_server:cast(?MODULE, {update_max_clients, Count}),
    ok.

update_max_clients_update_conflict_config(Config) ->
    gen_server:cast(?MODULE, {update_max_clients_update_conflict_config, Config}),
    ok.

print_full_message_history() ->
    gen_server:cast(?MODULE, print_full_message_history),
    ok.

get_connected_clients() -> call_server_and_print(get_connected_clients).

update_prev_messages_count_on_login(Count) ->
    gen_server:cast(?MODULE, {update_prev_messages_count_on_login, Count}),
    ok.

get_all_clients() -> call_server_and_print(get_all_clients).

get_chat_topic() -> call_server_and_print(get_chat_topic).

change_chat_topic(Topic) -> call_server_and_print({change_chat_topic, Topic}).

add_admin(Who) -> call_server_and_print({add_admin, Who}).

get_admins() -> call_server_and_print(get_admins).

kick_out(Who) -> call_server_and_print({kick_out, Who}).

mute(Who) -> call_server_and_print({mute, Who, 0}).

mute(Who, Seconds) -> call_server_and_print({mute, Who, Seconds}).

unmute(Who) -> call_server_and_print({unmute, Who}).

init(Configs) ->
    mnesia:start(),
    db:login_new_user({whereis(?MODULE), node()}, ?MODULE),
    db:add_new_admin(?MODULE),
    DefaultConfigs = #{
        max_clients => 5,
        max_clients_update_conflict_config => abort, %% kickout_all | abort
        prev_messages_count_on_login => 10,
        missed_private_messages_counts => #{},
        latest_msg_count => 0,
        chat_topic => 'General',
        chat_topic_set_by => ?MODULE
    },
    InitializedConfigs = lists:foldl(fun({K, V}, C) ->
        C#{K := V}
    end, DefaultConfigs, Configs),
    {ok, InitializedConfigs}.

handle_call(is_client_logged_in, From, State) ->
    Reply = db:is_client_logged_in(From),
    {reply, Reply, State};

handle_call({login, Who}, From, Configs) ->
    %% Constraint: No user with same username allowed
    {Reply, UpdatedConfigs} = case db:get_user_from_name(Who) of
        false ->
            %% user logged in for the first time
            %% Constraint: Only N (pre-configured) number of users allowed to login into the system
            ConnectedClientsList = db:get_connected_clients_list(),
            MaxClientsAllowed = maps:get(max_clients, Configs), 
            if 
                length(ConnectedClientsList) == MaxClientsAllowed ->  
                    {{stop, login_aborted_max_clients}, Configs};
                true ->
                    %% notify all connected clients about the new client entering.
                    gen_server:cast(?MODULE, {notify, {enter_user, {new, Who}}}),
                    {PrevMessages, UpdatedConfigs1} = get_previous_messages(Who, Configs, {enter_user, {new, Who}}),
                    db:login_new_user(From, Who),
                    {{login_success, PrevMessages}, UpdatedConfigs1}
            end;
        User ->
            case User#erchat_user.logged_in of
                false ->
                    %% user logging in back
                    %% TODO: Constraint to make sure that is the user is logging in back, then it should be from the same process.
                    %% notify all connected clients about the client entering back.
                    gen_server:cast(?MODULE, {notify, {enter_user, {back, Who}}}),
                    {PrevMessages, UpdatedConfigs1} = get_previous_messages(Who, Configs, {enter_user, {back, Who}}),
                    db:login_back_user(From, User),
                    {{login_success, PrevMessages}, UpdatedConfigs1};
                true ->
                    %% another user with same name already logged in
                    {{stop, login_aborted_same_name_exist}, Configs}
            end
    end,
    {reply, Reply, UpdatedConfigs};

handle_call(logout, From, Configs) ->
    User = db:get_user_from_node(From),
    gen_server:cast(?MODULE, {notify, {exit_user, User#erchat_user.name}}),
    db:logout_user(User),
    gen_server:cast({?CLIENT_NAME, User#erchat_user.node}, {stop, logged_out}),
    {reply, done, Configs};

handle_call({message, ToName, Msg}, From, Configs) ->
    FromUser = db:get_user_from_node(From),
    FromName = FromUser#erchat_user.name,
    MuteState = FromUser#erchat_user.muted,
    %% check if sender is muted
    MsgType = case ToName of
        ?MODULE -> group;
        _ -> private
    end,
    Message = create_message(FromName, ToName, Msg, MsgType, Configs),
    {Reply, UpdatedConfigs} = case MsgType of
        group ->
            case MuteState of 
                {true, Time} ->        
                    %% if muted, then reply error
                    R = case Time of
                        0 ->
                            you_are_muted;
                        _ ->
                            {you_are_muted_till, Time}
                    end,
                    {R, Configs};
                _ ->
                    %% else send the message
                    broadcast_message(Message, Configs)
            end;
        _ -> %% private
            private_message(Message, Configs)
    end,
    {reply, Reply, UpdatedConfigs};

handle_call(get_connected_clients, _From, Configs) ->
    ConnectedClientsList = db:get_connected_clients_list(),
    {reply, {connected_clients, ConnectedClientsList}, Configs};

handle_call(get_all_clients, _From, Configs) ->
    AllClientsList = db:get_all_clients_list(),
    {reply, {all_clients, AllClientsList}, Configs};

handle_call({add_admin, Who}, From, Configs) ->
    Reply = case db:is_admin(From) of
        false ->
            not_admin;
        FromName ->
            R = db:add_new_admin(Who),
            case R of
                done ->
                    gen_server:cast(?MODULE, {notify, {admin_promotion, Who, FromName}}),
                    done;
                _ ->
                    R
            end
    end,
    {reply, Reply, Configs};

handle_call(get_admins, _From, Configs) ->
    Reply = {admins, db:list_admins()},
    {reply, Reply, Configs};

handle_call(get_chat_topic, _From, Configs) ->
    Topic = maps:get(chat_topic, Configs),
    SetBy = maps:get(chat_topic_set_by, Configs),
    Reply = {current_chat_topic, Topic, set_by, SetBy},
    {reply, Reply, Configs};

handle_call({change_chat_topic, Topic}, From, Configs) ->
    {Reply, UpdatedConfigs} = case db:is_admin(From) of
        false ->
            {not_admin, Configs};
        _ ->
            case maps:get(chat_topic, Configs) of
                Topic ->
                    {chat_topic_update_fail_same_topic, Configs};
                _ ->
                    User = db:get_user_from_node(From),
                    FromName = User#erchat_user.name,
                    gen_server:cast(?MODULE, {notify, {topic_change, FromName, Topic}}),
                    {done, Configs#{chat_topic => Topic, chat_topic_set_by => FromName}}
            end
    end,
    {reply, Reply, UpdatedConfigs};

handle_call({kick_out, Who}, From, Configs) ->
    Reply = case db:is_admin(From) of
        false ->
            not_admin;
        FromName ->
            kick_out_user_handler(Who, FromName)
    end,
    {reply, Reply, Configs};

handle_call({mute, Who, Seconds}, From, Configs) ->
    Reply = case db:is_admin(From) of
        false ->
            not_admin;
        FromName ->
            case db:mute_user(Who, Seconds) of
                {done, MuteFinishTime} ->
                    {Notification, Info} = case Seconds of
                        0 ->
                            {
                                {user_muted, Who, FromName},
                                {info, you_are_muted}
                            };
                        _ ->
                            timer:apply_after(Seconds*1000, ?MODULE, unmute, [Who]),
                            {
                                {user_muted, Who, FromName, till, MuteFinishTime},
                                {info, {you_are_muted_till, MuteFinishTime}}
                            }
                    end,
                    gen_server:cast(?MODULE, {notify, Notification}),
                    MutedUser = db:get_user_from_name(Who),
                    gen_server:cast({?CLIENT_NAME, MutedUser#erchat_user.node}, Info),
                    done;
                R ->
                    R
            end
    end,
    {reply, Reply, Configs};

handle_call({unmute, Who}, From, Configs) ->
    Reply = case db:is_admin(From) of
        false ->
            not_admin;
        FromName ->
            case db:unmute_user(Who) of
                done ->
                    gen_server:cast(?MODULE, {notify, {user_unmuted, Who, FromName}}),
                    MutedUser = db:get_user_from_name(Who),
                    gen_server:cast({?CLIENT_NAME, MutedUser#erchat_user.node}, {info, you_are_unmuted}),
                    done;
                R ->
                    R
            end
    end,
    {reply, Reply, Configs};

handle_call(_Req, _From, Configs) ->
    Reply = reply,
    {reply, Reply, Configs}.



handle_cast({update_max_clients, Count}, Configs) ->
    LoggedInUsers = db:get_connected_clients_list(),
    UpdatedConfigs = if 
        length(LoggedInUsers) =< Count ->
            %% we can safely updated the max_clients
            Configs#{max_clients => Count};
        true ->
            %% we have to work according to the update_max_clients_config
            case maps:get(max_clients_update_conflict_config, Configs) of
                abort -> 
                    ui:print_info(max_clients_update_aborted),
                    Configs;
                kickout_all ->
                    [kick_out_user_handler(User, ?MODULE) || User <- LoggedInUsers],
                    Configs#{max_clients => Count}
            end
    end,
    {noreply, UpdatedConfigs};

handle_cast({update_max_clients_update_conflict_config, Config}, Configs) ->
    UpdatedConfigs = Configs#{max_clients_update_conflict_config := Config},
    {noreply, UpdatedConfigs};

handle_cast(print_full_message_history, Configs) ->
    Messages = db:get_all_messages(),
    UpdatedMessagesWithPrivateFor = update_message_type_to_private_for(Messages),
    ui:print_messages(lists:reverse(UpdatedMessagesWithPrivateFor)),
    {noreply, Configs};

handle_cast({update_prev_messages_count_on_login, Count}, Configs) ->
    UpdatedConfigs = Configs#{prev_messages_count_on_login := Count},
    {noreply, UpdatedConfigs};
    
handle_cast({notify, Notification}, Configs) ->
    Message = create_message(?MODULE, undefined, Notification, notification, Configs),
    {_, UpdatedConfigs} = broadcast_message(Message, Configs),
    {noreply, UpdatedConfigs};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





broadcast_message(Message, Configs) ->
    %% store the message in the database and update the config's latest message count for msg_id (for sorting of messages)
    db:store_message(Message),
    LMC = maps:get(latest_msg_count, Configs),
    UpdatedConfigs = Configs#{latest_msg_count := LMC + 1},
    %% send message to all clients for printing
    UserNodes = db:get_all_users_node(),
    case Message#erchat_message.content of
        {enter_user, {_, Who}} ->
            EnteredUser = db:get_user_from_name(Who),
            EnterUserNode = EnteredUser#erchat_user.node,
            [gen_server:cast({?CLIENT_NAME, UserNode}, {message_recieved, Message}) || UserNode <- UserNodes, UserNode =/= node(), UserNode =/= EnterUserNode];
        _ ->
            [gen_server:cast({?CLIENT_NAME, UserNode}, {message_recieved, Message}) || UserNode <- UserNodes, UserNode =/= node()]
    end,
    %% print message on the server ui
    ui:print_message(Message),  
    {done, UpdatedConfigs}.

private_message(Message, Configs) ->
    ToName = Message#erchat_message.to,
    case get_user_from_name(ToName) of
        false ->
            %% error
            {{no_user, ToName}, Configs};
        ToUser ->
            %% store the message in the database and update the config's latest message count for msg_id (for sorting of messages)
            db:store_message(Message),
            LMC = maps:get(latest_msg_count, Configs),
            UpdatedConfigs = Configs#{latest_msg_count := LMC + 1},

            %% send message to sender for printing
            FromUser = get_user_from_name(Message#erchat_message.from),
            FromUserNode = FromUser#erchat_user.node,
            gen_server:cast({?CLIENT_NAME, FromUserNode}, {message_recieved, Message#erchat_message{type = private_with_for}}),
            
            %% print message on the server ui
            ui:print_message(Message#erchat_message{type = private_with_for}),

            case ToUser#erchat_user.logged_in of
                true ->
                    %% send message to reciever if user is logged in
                    ToUserNode = ToUser#erchat_user.node,
                    gen_server:cast({?CLIENT_NAME, ToUserNode}, {message_recieved, Message}),
                    {done, UpdatedConfigs};
                false ->
                    %% else update the missed private messages count for the receiver in the configs
                    MissedPrivateMessagesCounts = maps:get(missed_private_messages_counts, Configs),
                    MissedPrivateMessagesCount = case maps:is_key(ToName, MissedPrivateMessagesCounts) of
                        false -> 
                            0;
                        true ->
                            maps:get(ToName, MissedPrivateMessagesCounts)
                    end,
                    UpdatedConfigs1 = UpdatedConfigs#{
                        missed_private_messages_counts => MissedPrivateMessagesCounts#{
                            ToName => MissedPrivateMessagesCount + 1
                        }
                    },
                    {{user_not_logged_in_private_message_saved, ToName}, UpdatedConfigs1}
            end
    end.

create_message(FromName, ToName, Msg, MsgType, Configs) ->
    LMC = maps:get(latest_msg_count, Configs),
    #erchat_message{
        msg_id = LMC + 1,
        time = erlang:localtime(),
        from = FromName,
        to = ToName,
        content = Msg,
        type = MsgType
    }.

get_previous_messages(Who, Configs, EnteringUserMsg) ->
    PrevMessagesCount = maps:get(prev_messages_count_on_login, Configs),
    MissedPrivateMessagesCount = get_missed_private_messages_count(Who, Configs),
    UpdatedConfigs = clear_missed_private_messages_count(Who, Configs),
    PrevMessages = get_previous_n_messages(lists:reverse(db:get_all_messages()), PrevMessagesCount, MissedPrivateMessagesCount, []),
    LatestChatTopicNotification = get_chat_topic_notification(Configs),
    EnteringUserMessage = create_message(?MODULE, ?MODULE, EnteringUserMsg, notification, Configs),
    PrevMessagesWithChatTopic = PrevMessages ++ [EnteringUserMessage, LatestChatTopicNotification], 
    {PrevMessagesWithChatTopic, UpdatedConfigs}.

get_missed_private_messages_count(Who, Configs) ->
    MissedPrivateMessagesCounts = maps:get(missed_private_messages_counts, Configs),
    case maps:is_key(Who, MissedPrivateMessagesCounts) of
        false -> 
            0;
        true ->
            maps:get(Who, MissedPrivateMessagesCounts)
    end.

clear_missed_private_messages_count(Who, Configs) ->
    MissedPrivateMessagesCounts = maps:get(missed_private_messages_counts, Configs),
    Configs#{
        missed_private_messages_counts => MissedPrivateMessagesCounts#{Who => 0}
    }.

get_chat_topic_notification(Configs) ->
    Topic = maps:get(chat_topic, Configs),
    SetBy = maps:get(chat_topic_set_by, Configs),
    Msg = {current_chat_topic, Topic, set_by, SetBy},
    create_message(?MODULE, undefined, Msg, notification, Configs).

get_previous_n_messages([], _, _, Acc) -> Acc;
get_previous_n_messages(_, 0, 0, Acc) -> Acc;
get_previous_n_messages([Message | Rest], B, P, Acc) ->
    case Message#erchat_message.type of
        notification ->
            get_previous_n_messages(Rest, B, P, [Message | Acc]);
        private ->
            if
                P > 0 -> 
                    get_previous_n_messages(Rest, B, P - 1, [Message | Acc]);
                true ->
                    get_previous_n_messages(Rest, B-1, 0, [Message | Acc])
            end;        
        _ ->
            if 
                B > 0 ->
                    get_previous_n_messages(Rest, B - 1, P, [Message | Acc]);
                true ->
                    get_previous_n_messages(Rest, 0, P, Acc)
            end
    end.

call_server_and_print(Req) ->
    Reply = gen_server:call(?MODULE, Req),
    ui:print_info(Reply).

kick_out_user_handler(Who, By) ->
    case db:kick_out_user(Who) of
        done ->
            gen_server:cast(?MODULE, {notify, {kick_out_user, Who, By}}),
            KickedOutUser = db:get_user_from_name(Who),
            gen_server:cast({?CLIENT_NAME, KickedOutUser#erchat_user.node}, {stop, kicked_out}),
            done;
        R ->
            R
    end.

update_message_type_to_private_for(Messages) ->
    F = fun(M) ->
        case M#erchat_message.type of
            private ->
                M#erchat_message{type = private_with_for};
            _ ->
                M
        end
    end,
    [F(Message) || Message <- Messages].