-module(chatli_user_controller).
-export([
         user/1,
         login/1,
         signup/1,
         manage_user/1,
         device/1,
         manage_device/1,
         delete_user/1
        ]).

-include("chatli.hrl").

user(#{req := #{ method := <<"GET">>}}) ->
    {ok, Users} = chatli_db:get_all_users(),
    {json, 200, #{}, Users}.

signup(#{req := #{ method := <<"POST">>},
       json := JSON}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    logger:debug("json: ~p", [JSON]),
    Phonenumber = maps:get(<<"phoneNumber">>, JSON, <<>>),
    Email = maps:get(<<"email">>, JSON, <<>>),
    Password = maps:get(<<"password">>, JSON, undefined),
    Username = maps:get(<<"username">>, JSON),
    Object = #{id => Id,
               username => Username,
               phone_number => Phonenumber,
               email => Email,
               password => Password},
    case {Phonenumber, Email, Password} of
        {undefined, undefined, undefined} ->
            {status, 400};
        {_, _, undefined} ->
            {status, 400};
        _ ->
            chatli_db:create_user(Object),
            {status, 201}
    end.

login(#{req := #{method := <<"POST">>},
        json := JSON}) ->
    Username = maps:get(<<"username">>, JSON, undefined),
    Password = maps:get(<<"password">>, JSON, undefined),
    case {Username, Password} of
        {undefined, undefined} ->
            {status, 400};
        {_, undefined} ->
            {status, 400};
        {undefined, _} ->
            {status, 400};
        {Username, Password} ->
            {ok, User} = chatli_db:get_login(Username, Password),
            AuthObj = #{access_token => jwerl:sign(maps:remove(password, User), hs512, ?SECRET)},
            {json, 200, #{}, AuthObj}
    end.

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

delete_user(#{req := #{method := <<"DELETE">>,
                       bindings := #{userid := UserId}}}) ->
    chatli_db:delete_user(UserId),
    {status, 200}.
