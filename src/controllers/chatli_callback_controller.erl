-module(chatli_callback_controller).

-export([create_callback/1,
         manage_callback/1]).

create_callback(#{req := #{method := <<"POST">>},
                  json := #{<<"type">> := Type,
                            <<"value">> := Value,
                            <<"url">> := Url}}) ->
    Id = chatli_uuid:get_v4(),
    case chatli_user_db:find(Type, Value) of
        undefined ->
            {status, 404};
        {ok, #{id := UserId}} ->
            ok = chatli_db:create_callback(Id, UserId, Url),
            {json, 200, #{}, #{id => Id}}
    end.

manage_callback(#{req := #{method := <<"GET">>,
                           bindings := #{callbackid := CallbackId}}}) ->
    {ok, Result} = chatli_db:get_callback(CallbackId),
    {json, 200, #{}, Result};
manage_callback(#{req := #{method := <<"DELETE">>,
                           bindings := #{callbackid := CallbackId}}}) ->
    chatli_db:delete_callback(CallbackId),
    {status, 200}.