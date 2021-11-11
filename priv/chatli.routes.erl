#{prefix => "/v1",
  security => false,
  routes => [
            {"/signup", { chatli_user_controller, signup}, #{methods => [post]}},
            {"/login", { chatli_user_controller, login}, #{methods => [post]}},
            {"/callback", { chatli_callback_controller, create_callback}, #{method => [post]}},
            {"/callback/:callbackid", { chatli_callback_controller, get_callback}, #{method => [get]}},
            {"/callback/:callbackid", { chatli_callback_controller, delete_callback}, #{method => [delete]}},
            {"/history", {chatli_chat_controller, get_history}, #{method => [post]}},
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
            {"/chat", {chatli_chat_controller, get_chats}, #{methods => [get]}},
            {"/chat", {chatli_chat_controller, create_chat}, #{methods => [post]}},
            {"/chat/:chatid", {chatli_chat_controller, get_chat}, #{methods => [get]}},
            {"/chat/:chatid", {chatli_chat_controller, delete_chat}, #{methods => [delete]}},
            {"/chat/:chatid/attachment/:attachmentid", {chatli_chat_controller, get_attachment}, #{methods => [get]}},
            {"/chat/:chatid/message", {chatli_chat_controller, get_archive}, #{methods => [get]}},
            {"/chat/:chatid/message/:messageid", {chatli_chat_controller, get_message}, #{methods => [get]}},
            {"/chat/:chatid/message/:messageid", {chatli_chat_controller, delete_message}, #{methods => [delete]}},
            {"/chat/:chatid/participant", {chatli_chat_controller, get_participants}, #{methods => [get]}},
            {"/chat/:chatid/participant", {chatli_chat_controller, add_participants}, #{methods => [post]}},
            {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, get_participants}, #{methods => [get]}},
            {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, update_participants}, #{methods => [put]}},
            {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, delete_participants}, #{methods => [delete]}},
            {"/device", {chatli_user_controller, device}, #{methods => [get]}},
            {"/device/:deviceid", {chatli_user_controller, get_device}, #{methods => [get]}},
            {"/device/:deviceid", {chatli_user_controller, update_device}, #{methods => [put]}},
            {"/device/:deviceid", {chatli_user_controller, delete_device}, #{methods => [delete]}},
            {"/device/:deviceid/user/:userid/ws", chatli_ws_client, #{protocol => ws, idle_timeout => 15000}}
]}.