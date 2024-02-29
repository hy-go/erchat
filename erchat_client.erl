-module(erchat_client).
-behaviour(gen_server).

-export([
    login/1,
    logout/0,
    message/1,
    message/2,
    get_connected_clients/0,
    add_admin/1,
    get_admins/0,
    get_chat_topic/0,
    change_chat_topic/1,
    kick_out/1,
    mute/1,
    mute/2,
    unmute/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(ui, [
    print_info/1
]).

-define(SERVER_NAME, erchat_server).
-define(SERVER_NODE, 'server@ggn001749.greyorange.com').
-define(SERVER, {?SERVER_NAME, ?SERVER_NODE}).

%% User Commands
login(Who) ->
    case is_client_logged_in() of
        true -> 
            ui:print_info(login_fail_another_user_logged_in);
        false ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
            Reply = gen_server:call(?SERVER, {login, Who}),
            case Reply of
                {login_success, PrevMessages} ->
                    ui:print_messages(PrevMessages);
                _ ->
                    gen_server:cast(?MODULE, Reply)
            end
    end.

logout() -> request_if_logged_in(logout).

message(Message) -> message(?SERVER_NAME, Message).

message(ToName, Message) -> request_if_logged_in({message, ToName, Message}).

get_connected_clients() -> request_if_logged_in(get_connected_clients).

add_admin(Who) -> request_if_logged_in({add_admin, Who}).

get_admins() -> request_if_logged_in(get_admins).

get_chat_topic() -> request_if_logged_in(get_chat_topic).

change_chat_topic(Topic) -> request_if_logged_in({change_chat_topic, Topic}).

kick_out(Who) -> request_if_logged_in({kick_out, Who}).

mute(Who) -> mute(Who, 0).

mute(Who, Seconds) -> request_if_logged_in({mute, Who, Seconds}).

unmute(Who) -> request_if_logged_in({unmute, Who}).


init([]) ->
    {ok, #{}}.

handle_call(_Req, _From, State) ->
    Reply = reply,
    {reply, Reply, State}.

handle_cast({message_recieved, Message}, State) ->
    ui:print_message(Message),
    {noreply, State};

handle_cast({info, Info}, State) ->
    ui:print_info(Info),
    {noreply, State};
    
handle_cast({stop, Reason}, _State) ->
    ui:print_info(Reason),
    {stop, normal, ok_state};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% middlewares
is_client_logged_in() ->
    gen_server:call(?SERVER, is_client_logged_in).
    
request_if_logged_in(Req) ->
    case is_client_logged_in() of
        true ->
            Reply = gen_server:call(?SERVER, Req),
            ui:print_info(Reply);
        false ->
            ui:print_info(not_logged_in)
    end.