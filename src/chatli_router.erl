-module(chatli_router).

-export([routes/1]).

routes(_) ->
    [
        #{
            prefix => "/v1",
            security => false,
            routes => [
                {"/signup", fun chatli_user_controller:signup/1, #{methods => [options, post]}},
                {"/login", fun chatli_user_controller:login/1, #{methods => [options, post]}},
                {"/callback", fun chatli_callback_controller:create_callback/1, #{
                    method => [options, post]
                }},
                {"/callback/:callbackid", fun chatli_callback_controller:get_callback/1, #{
                    method => [options, get]
                }},
                {"/callback/:callbackid", fun chatli_callback_controller:delete_callback/1, #{
                    method => [options, delete]
                }},
                {"/history", fun chatli_chat_controller:get_history/1, #{method => [options, post]}},
                {"/chat/:chatid/attachment/:attachmentid",
                    fun chatli_chat_controller:get_attachment_no_auth/1, #{
                        method => [options, get]
                    }},
                {"/heartbeat", fun(_) -> {status, 200} end, #{method => [get]}}
            ]
        },

        #{
            prefix => "/client",
            security => fun chatli_auth:auth_jwt/1,
            routes => [
                {"/user", fun chatli_user_controller:user/1, #{methods => [options, get]}},
                {"/user/:userid", fun chatli_user_controller:delete_user/1, #{
                    methods => [options, delete]
                }},
                {"/message", fun chatli_chat_controller:message/1, #{methods => [options, post]}},
                {"/chat", fun chatli_chat_controller:get_chats/1, #{methods => [options, get]}},
                {"/chat", fun chatli_chat_controller:create_chat/1, #{methods => [options, post]}},
                {"/chat/:chatid", fun chatli_chat_controller:get_chat/1, #{
                    methods => [options, get]
                }},
                {"/chat/:chatid", fun chatli_chat_controller:delete_chat/1, #{
                    methods => [options, delete]
                }},
                {"/chat/:chatid/attachment/:attachmentid",
                    fun chatli_chat_controller:get_attachment/1, #{methods => [options, get]}},
                {"/chat/:chatid/message", fun chatli_chat_controller:get_archive/1, #{
                    methods => [options, get]
                }},
                {"/chat/:chatid/message/:messageid", fun chatli_chat_controller:manage_message/1, #{
                        methods => [options, get, delete]
                    }},
                {"/chat/:chatid/participant", fun chatli_chat_controller:participants/1, #{
                    methods => [options, get, post]
                }},
                {"/chat/:chatid/participant/:participantid",
                    fun chatli_chat_controller:manage_participants/1, #{
                        methods => [options, get, put, delete]
                    }},
                {"/device", fun chatli_user_controller:device/1, #{methods => [options, get]}},
                {"/device/:deviceid", fun chatli_user_controller:manage_device/1, #{
                    methods => [options, get, put, delete]
                }}
            ]
        },
        #{
            prefix => "/client",
            security => false,
            routes => [
                {"/device/:deviceid/user/:userid/ws", chatli_ws_client, #{
                    protocol => ws, idle_timeout => 15000
                }}
            ]
        }
    ].
