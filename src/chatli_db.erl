-module(chatli_db).

-export([create_user/1,
         get_user/1,
         get_login/2,
         find_user/2,
         delete_user/1,
         get_all_users/0,
         get_all_other_users/1,
         create_message/1,
         get_message/2,
         get_chat_messages/1,
         create_chat/1,
         get_chat/1,
         get_all_chats/1,
         delete_chat/1,
         add_participant/2,
         remove_participant/2,
         get_participants/1,
         upsert_device/3,
         get_device/2,
         get_all_devices/1,
         delete_device/2,
         create_callback/3,
         get_callback/1,
         get_user_callbacks/1,
         delete_callback/1]).

create_user(#{id := Id,
              username := Username,
              phone_number := PhoneNumber,
              email := Email,
              password := Password}) ->
    SQL = <<"INSERT INTO chatli_user (id, username, phone_number, email, password) VALUES ($1, $2, $3, $4, $5)">>,
    query1(SQL, [Id, Username, PhoneNumber, Email, Password]).

get_user(UserId) ->
    SQL = <<"SELECT id, username, phone_number, email FROM chatli_user WHERE id = $1">>,
    query1(SQL, [UserId]).

get_login(Username, Password) ->
    SQL = <<"SELECT * FROM chatli_user WHERE username = $1 AND password = $2">>,
    query1(SQL, [Username, Password]).

find_user(Type, Value) ->
    Base = <<"SELECT * FROM chatli_user WHERE ">>,
    WhereSQL = case Type of
                  <<"email">> ->
                    <<"email = $1">>;
                  <<"phone_number">> ->
                    <<"phone_number = $1">>
               end,
    query1(<<Base/binary, WhereSQL/binary>>, [Value]).

delete_user(UserId) ->
    SQL = <<"DELETE FROM chatli_user WHERE id = $1">>,
    query1(SQL, [UserId]).

get_all_users() ->
    SQL = <<"SELECT id, avatar, email, phone_number, username FROM chatli_user">>,
    query(SQL, []).

get_all_other_users(UserId) ->
    SQL = <<"SELECT id, avatar, email, phone_number, username FROM chatli_user WHERE id != $1">>,
    query(SQL, [UserId]).

create_message(#{<<"id">> := Id,
                 <<"chatId">> := ChatId,
                 <<"payload">> := Payload,
                 <<"sender">> := UserId,
                 <<"timestamp">> := Timestamp}) ->
    SQL = <<"INSERT INTO message (id, chat_id, payload, sender, timestamp) VALUES ($1, $2, $3, $4, $5)">>,
    query1(SQL, [Id, ChatId, Payload, UserId, Timestamp]).

get_message(ChatId, MessageId) ->
    SQL = <<"SELECT id,
                    chat_id,
                    payload,
                    sender,
                    DATE_PART('epoch', timestamp)
            FROM message
            WHERE chat_id = $1 AND id = $2">>,
    query1(SQL, [ChatId, MessageId]).

get_chat_messages(ChatId) ->
    SQL = <<"SELECT id,
                    chat_id,
                    payload,
                    sender,
                    DATE_PART('epoch', timestamp)
            FROM message
            WHERE chat_id = $1">>,
    query(SQL, [ChatId]).

create_chat(#{<<"id">> := Id,
              <<"name">> := Name,
              <<"description">> := Description}) ->
    SQL = <<"INSERT INTO chat (id, name, description) VALUES ($1, $2, $3)">>,
    query1(SQL, [Id, Name, Description]).

get_chat(ChatId) ->
    SQL = <<"SELECT * FROM chat WHERE id = $1">>,
    query1(SQL, [ChatId]).

get_all_chats(UserId) ->
    SQL = <<"SELECT *
             FROM chat
             INNER JOIN participant
                ON paricipant.user_id = $1
             WHERE participant.chat_id = chat.id">>,
    query(SQL, [UserId]).

delete_chat(ChatId) ->
  SQL = <<"DELETE FROM chat WHERE id = $1">>,
  query1(SQL, [ChatId]).

add_participant(ChatId, UserId) ->
    SQL = <<"INSERT INTO participant (chat_id, user_id) VALUES ($1, $2)">>,
    query1(SQL, [ChatId, UserId]).

remove_participant(ChatId, UserId) ->
    SQL = <<"DELETE FROM participant WHERE chat_id = $1 AND user_id = $2">>,
    query1(SQL, [ChatId, UserId]).

get_participants(ChatId) ->
    SQL = <<"SELECT user_id FROM participant WHERE chat_id = $1">>,
    query(SQL, [ChatId]).

upsert_device(DeviceId, UserId, Name) ->
  delete_device(DeviceId, UserId),
    SQL = <<"INSERT INTO
               device
               (
                 id,
                 user_id,
                 name
               ) VALUES (
                 $1,
                 $2,
                 $3
               )">>,
    query1(SQL, [DeviceId, UserId, Name]).

delete_device(DeviceId, UserId) ->
    SQL = <<"DELETE FROM
                device
            WHERE
                id = $1 AND
                user_id = $2">>,
    query(SQL, [DeviceId, UserId]).

get_device(DeviceId, UserId) ->
    SQL = <<"SELECT id, name FROM device WHERE id = $1 AND user_id = $2">>,
    query1(SQL, [DeviceId, UserId]).

get_all_devices(UserId) ->
    SQL = <<"SELECT id, name FROM device WHERE user_id = $1">>,
    query(SQL, [UserId]).

create_callback(CallbackId, UserId, Url) ->
    SQL = <<"INSERT INTO callback
                (
                  id,
                  user_id,
                  url
                )
             VALUES
               (
                 $1,
                 $2,
                 $3
               )">>,
    query1(SQL, [CallbackId, UserId, Url]).

get_callback(CallbackId) ->
    SQL = <<"SELECT * FROM callback WHERE id = $1">>,
    query1(SQL, [CallbackId]).

get_user_callbacks(UserId) ->
    SQL = <<"SELECT url FROM callback where user_id = $1">>,
    query(SQL, [UserId]).

delete_callback(CallbackId) ->
    SQL = <<"DELETE FROM callback WHERE id = $1">>,
    query1(SQL, [CallbackId]).

% Expect 1 result
query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := 1} -> ok;
        #{command := select,
          rows := []} -> undefined;
        #{command := select,
          rows := [Row]} -> {ok, Row};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := 1} -> ok;
        #{command := delete} -> undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := Num} -> {ok, Num};
        #{command := select,
          rows := Rows} -> {ok, Rows};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := Num} -> {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.