-module(chatli_callback_controller).

-export([create_callback/1,
         manage_callback/1]).

create_callback(#{req := #{method := <<"POST">>},
                  json := #{<<"userId">> := UserId,
                            <<"url">> := Url}}) ->
    Id = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),

    ok = chatli_db:create_callbacks(Id, UserId, Url),
    {json, 201, #{}, #{id => Id}}.

manage_callback(#{req := #{method := <<"GET">>,
                           bindings := #{callbackid := CallbackId}}}) ->
    {ok, Result} = chatlid_db:get_callback(CallbackId),
    {json, 200, #{}, Result};
manage_callback(#{req := #{method := <<"DELETE">>,
                           bindings := #{callbackid := CallbackId}}}) ->
    chatli_db:delete_callback(CallbackId),
    {status, 200}.