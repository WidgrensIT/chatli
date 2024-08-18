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

user(#{
    method := <<"GET">>,
    auth_data := #{id := UserId}
}) ->
    {ok, Users} = chatli_user_db:get_all_other(UserId),
    {json, 200, #{}, Users}.

signup(#{
    method := <<"POST">>,
    json := JSON
}) ->
    Id = chatli_uuid:get_v4(),
    Phonenumber = maps:get(<<"phoneNumber">>, JSON, <<>>),
    Email = maps:get(<<"email">>, JSON, <<>>),
    Password = maps:get(<<"password">>, JSON, undefined),
    Username = maps:get(<<"username">>, JSON),
    Object = #{
        id => Id,
        username => Username,
        phone_number => Phonenumber,
        email => Email,
        password => Password
    },
    case {Phonenumber, Email, Password} of
        {undefined, undefined, undefined} ->
            {status, 400};
        {_, _, undefined} ->
            {status, 400};
        _ ->
            chatli_user_db:create(Object),
            {status, 201}
    end.

login(#{
    method := <<"POST">>,
    json := #{
        <<"username">> := Username,
        <<"password">> := Password
    }
}) ->
    case chatli_user_db:get_login(Username, Password) of
        {ok, User} ->
            AuthObj = #{access_token => jwerl:sign(maps:remove(password, User), hs512, ?SECRET)},
            {json, 200, #{}, AuthObj};
        _ ->
            {status, 401}
    end;
login(_) ->
    {status, 401}.

manage_user(#{
    req := #{
        method := <<"GET">>,
        bindings := #{<<"userid">> := UserId}
    }
}) ->
    case chatli_user_db:get(UserId) of
        {ok, User} ->
            {json, 200, #{}, User};
        Error ->
            logger:warning("get user: ~p", [Error]),
            {status, 500}
    end;
manage_user(#{
    req := #{
        method := <<"PUT">>,
        bindings := #{userid := UserId}
    }
}) ->
    %% Change name on user or phone number
    {json, 200, #{}, #{id => UserId}}.

device(#{
    method := <<"GET">>,
    auth_data := #{id := UserId}
}) ->
    {ok, Result} = chatli_device_db:get_all(UserId),
    {json, 200, #{}, Result}.

manage_device(#{
    method := <<"PUT">>,
    bindings := #{<<"deviceid">> := DeviceId},
    auth_data := #{id := UserId},
    json := #{<<"name">> := Name}
}) ->
    ok = chatli_device_db:upsert(DeviceId, UserId, Name),
    {status, 200};
manage_device(#{
    method := <<"GET">>,
    bindings := #{<<"deviceid">> := DeviceId},
    auth_data := #{id := UserId}
}) ->
    case chatli_device_db:get(DeviceId, UserId) of
        {ok, Result} ->
            {json, 200, #{}, Result};
        _ ->
            {status, 404}
    end;
manage_device(#{
    method := <<"DELETE">>,
    bindings := #{<<"deviceid">> := DeviceId},
    auth_data := #{id := UserId}
}) ->
    chatli_device_db:delete(DeviceId, UserId),
    {status, 200}.

delete_user(#{
    method := <<"DELETE">>,
    auth_data := #{id := UserId}
}) ->
    chatli_user_db:delete(UserId),
    {status, 200}.
