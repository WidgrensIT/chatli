#{prefix => "/v1",
  security => false,
  routes => [
            {"/signup", { chatli_user_controller, signup}, #{methods => [post]}},
            {"/login", { chatli_user_controller, login}, #{methods => [post]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.

#{prefix => "/client/",
  security => {chatli_auth, auth_jwt},
  routes => [
            {"/", { chatli_user_controller, manage_user}, #{methods => [get, put, delete]}},
            {"/user", {chatli_user_controller, user}, #{methods => [get]}},
            {"/user/:userid", {chatli_user_controller, delete_user}, #{methods => [delete]}},
            {"/message", {chatli_chat_controller, message}, #{methods => [post]}},
            {"/chat", {chatli_chat_controller, chat}, #{methods => [get, post]}},
            {"/chat/:chatid", {chatli_chat_controller, manage_chat}, #{methods => [get, delete]}},
            {"/chat/:chatid/message", {chatli_chat_controller, get_archive}, #{methods => [get]}},
            {"/chat/:chatid/message/:messageid", {chatli_chat_controller, manage_message}, #{methods => [get, delete]}},
            {"/chat/:chatid/participant", {chatli_chat_controller, participants}, #{methods => [get, post]}},
            {"/chat/:chatid/participant/:participantid", {chatli_chat_controller, manage_participants}, #{methods => [get, put, delete]}},
            {"/device", {chatli_user_controller, device}, #{methods => [get]}},
            {"/device/:deviceid", {chatli_user_controller, manage_device}, #{methods => [get, put]}}
]}.
