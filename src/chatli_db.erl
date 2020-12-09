-module(chatli_db).

-export([create_message/1,
         get_message/2,
         get_chat_messages/1,
         create_chat/1,
         get_chat/1,
         get_all_chats/1,
         get_dm_chat/2,
         delete_chat/1,
         add_participant/2,
         remove_participant/2,
         get_participants/1,
         get_all_other_participants/2,
         create_callback/3,
         get_callback/1,
         get_user_callbacks/1,
         delete_callback/1,
         query/2,
         query1/2]).



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
              <<"description">> := Description,
              <<"type">> := Type}) ->
    SQL = <<"INSERT INTO chat (id, name, description, type) VALUES ($1, $2, $3, $4)">>,
    query1(SQL, [Id, Name, Description, Type]).

get_chat(ChatId) ->
    SQL = <<"SELECT * FROM chat WHERE id = $1">>,
    query1(SQL, [ChatId]).

get_dm_chat(User1, User2) ->
    SQL = <<"SELECT chat.*
             FROM chat
             INNER JOIN participant AS p1 ON p1.user_id = $1 AND p1.chat_id = chat.id
             INNER JOIN participant AS p2 ON p2.user_id = $2 AND p2.chat_id = chat.id
             WHERE chat.type = '1to1' LIMIT 1">>,
    query1(SQL, [User1, User2]).

get_all_chats(UserId) ->
    SQL = <<"SELECT chat.*
             FROM chat
             INNER JOIN participant
                ON participant.user_id = $1 AND
                   participant.chat_id = chat.id">>,
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

get_all_other_participants(ChatId, UserId) ->
    SQL = <<"SELECT chatli_user.id,
                    chatli_user.username,
                    chatli_user.email
             FROM participant
             INNER JOIN chatli_user ON chatli_user.id = participant.user_id
             WHERE participant.chat_id = $1 AND participant.user_id != $2">>,
    query(SQL, [ChatId, UserId]).

get_participants(ChatId) ->
    SQL = <<"SELECT user_id FROM participant WHERE chat_id = $1">>,
    query(SQL, [ChatId]).



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
