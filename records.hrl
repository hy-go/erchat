-record(erchat_user, {
    node :: atom(),
    name :: atom(),
    user_type = normal :: normal | admin,
    muted = {false, 0} :: {boolean(), 0 | calendar:datetime()},
    logged_in = true :: boolean()
}).

-record(erchat_message, { 
    msg_id :: integer(),
    time :: calendar:datetime(),
    content :: any(),
    from :: atom(),
    to = undefined :: undefined | atom(),
    type :: private | notification | group
}).