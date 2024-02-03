-module(chatli_router).

-export([routes/1]).

routes(_) ->
    [#{prefix => "/v1",
       security => false,
       routes => [
                  {"/signup", { chatli_user_controller, signup}, #{methods => [options, post]}},
                  {"/login", { chatli_user_controller, login}, #{methods => [options, post]}},
                  {"/callback", { chatli_callback_controller, create_callback}, #{method => [options, post]}},
                  {"/callback/:callbackid", { chatli_callback_controller, manage_callback}, #{method => [options, get, delete]}},
                  {"/history", {chatli_chat_controller, get_history}, #{method => [options, post]}},
                  {"/chat/:chatid/attachment/:attachmentid", { chatli_chat_controller, get_attachment_no_auth}, #{method => [options, get]}},
                  {"/assets/[...]", "assets"}
                ]
       },

  #{prefix => "/client",
    security => {chatli_auth, auth_jwt},
    routes => [
              {"/user", {chatli_user_controller, user}, #{methods => [options, get]}},
              {"/user/:userid", {chatli_user_controller, delete_user}, #{methods => [options, delete]}},
              {"/message", {chatli_chat_controller, message}, #{methods => [options, post]}},
              {"/chat", {chatli_chat_controller, get_chat}, #{methods => [options, get]}},
              {"/chat", {chatli_chat_controller, create_chat}, #{methods => [options, post]}},
              {"/chat/:chatid", {chatli_chat_controller, manage_chat}, #{methods => [options, get, delete]}},
              {"/chat/:chatid/attachment/:attachmentid", {chatli_chat_controller, get_attachment}, #{methods => [options, get]}},
              {"/chat/:chatid/message", {chatli_chat_controller, get_archive}, #{methods => [options, get]}},
              {"/chat/:chatid/message/:messageid", {chatli_chat_controller, manage_message}, #{methods => [options, get, delete]}},
              {"/chat/:chatid/participant", {chatli_chat_controller, participants}, #{methods => [options, get, post]}},
              {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, manage_participants}, #{methods => [options, get, put, delete]}},
              {"/device", {chatli_user_controller, device}, #{methods => [options, get]}},
              {"/device/:deviceid", {chatli_user_controller, manage_device}, #{methods => [options, get, put, delete]}}
  ]},
  #{prefix => "/client",
    security => false,
    routes => [{"/device/:deviceid/user/:userid/ws", chatli_ws_client, #{protocol => ws, idle_timeout => 15000}}]}].

