#{prefix => "/v1",
  security => false,
  routes => [
            {"/signup", { chatli_user_controller, signup}, #{methods => [post]}},
            {"/login", { chatli_user_controller, login}, #{methods => [post]}},
            {"/callback", { chatli_callback_controller, create_callback}, #{method => [post]}},
            {"/callback/:callbackid", { chatli_callback_controller, manage_callback}, #{method => [get, delete]}},
            {"/chat/:chatid/attachment/:attachmentid", { chatli_chat_controller, get_attachment_no_auth}, #{method => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.

#{prefix => "/client",
  security => {chatli_auth, auth_jwt},
  routes => [
            {"/user", {chatli_user_controller, user}, #{methods => [get]}},
            {"/user/:userid", {chatli_user_controller, delete_user}, #{methods => [delete]}},
            {"/message", {chatli_chat_controller, message}, #{methods => [post]}},
            {"/chat", {chatli_chat_controller, chat}, #{methods => [get, post]}},
            {"/chat/:chatid", {chatli_chat_controller, manage_chat}, #{methods => [get, delete]}},
            {"/chat/:chatid/attachment/:attachmentid", {chatli_chat_controller, get_attachment}, #{methods => [get]}},
            {"/chat/:chatid/message", {chatli_chat_controller, get_archive}, #{methods => [get]}},
            {"/chat/:chatid/message/:messageid", {chatli_chat_controller, manage_message}, #{methods => [get, delete]}},
            {"/chat/:chatid/participant", {chatli_chat_controller, participants}, #{methods => [get, post]}},
            {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, manage_participants}, #{methods => [get, put, delete]}},
            {"/device", {chatli_user_controller, device}, #{methods => [get]}},
            {"/device/:deviceid", {chatli_user_controller, manage_device}, #{methods => [get, put, delete]}},
            {"/device/:deviceid/user/:userid/ws", chatli_ws_client, #{protocol => ws, idle_timeout => 15000}}
]}.