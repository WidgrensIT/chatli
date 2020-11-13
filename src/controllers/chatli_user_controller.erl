-module(chatli_user_controller).
-export([
         login/1,
         signup/1,
         manage_user/1,
         device/1,
         manage_device/1
        ]).

login(#{req := #{method := <<"POST">>},
        json := #{<<"username">> := _Username,
                  <<"password">> := _Password}}) ->
    {json, #{<<"token">> => <<"123">>}}.


signup(#{req := #{ method := <<"GET">>}}) ->
    {json, 200, #{}, []};
signup(#{req := #{ method := <<"POST">>},
       json := JSON}) ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    logger:debug("json: ~p", [JSON]),
    Object = maps:merge(#{<<"id">> => Id}, JSON),
    chatli_db:create_user(Object),
    {json, 201, #{}, Object}.

manage_user(#{req := #{method := <<"GET">>,
                       bindings := #{userid := UserId}}}) ->
    case chatli_db:get_user(UserId) of
        {ok, User} ->
            {json, 200, #{}, User};
        Error ->
            logger:warning("get user: ~p", [Error]),
            {status, 500}
    end;
manage_user(#{ req := #{method := <<"PUT">>,
                        bindings := #{userid := UserId }}}) ->
    %% Change name on user or phone number
    {json, 200, #{}, #{id => UserId}}.

device(#{req := #{method := <<"GET">>}}) ->
    %% Get users devices
    {json, 200, #{}, []}.

manage_device(#{req := #{ method := <<"PUT">>,
                          bindings := #{deviceid := DeviceId }}}) ->
    %% Upsert the device and token
    {json, 200, #{}, #{id => DeviceId}};
manage_device(#{req := #{method := <<"GET">>,
                         bindings := #{devicdeid := DeviceId}}}) ->
    %% Get device
    {json ,200, #{}, #{id => DeviceId}}.
