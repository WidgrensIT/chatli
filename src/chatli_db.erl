-module(chatli_db).

-export([create_user/1,
         get_user/1,
         find_user/1,
         delete_user/1,
         get_all_users/0,
         create_message/1,
         get_message/1,
         get_chat_messages/1,
         create_chat/1,
         get_chat/1,
         get_all_chats/1,
         add_participant/2,
         remove_participant/2,
         get_participants/1]).

create_user(#{id := Id,
              username := Username,
              phone_number := PhoneNumber}) ->
    SQL = <<"INSERT INTO chatli_user (id, username, phone_number) VALUES ($1, $2, $3)">>,
    query1(SQL, [Id, Username, PhoneNumber]).

get_user(UserId) ->
    SQL = <<"SELECT * FROM chatli_user WHERE id = $1">>,
    query1(SQL, [UserId]).

find_user(PhoneNumber) ->
    SQL = <<"SELECT * FROM chatli_user WHERE phone_number = $1">>,
    query1(SQL, [PhoneNumber]).

delete_user(UserId) ->
    SQL = <<"DELETE FROM chatli_user WHERE id = $1">>,
    query1(SQL, [UserId]).

get_all_users() ->
    SQL = <<"SELECT * FROM chatli_user">>,
    query(SQL, []).

create_message(#{<<"id">> := Id,
                 <<"chat_id">> := ChatId,
                 <<"payload">> := Payload,
                 <<"sender">> := UserId,
                 <<"timestamp">> := Timestamp}) ->
    SQL = <<"INSERT INTO message (id, chat_id, payload, sender, timestamp) VALUES ($1, $2, $3, $4, $5)">>,
    query1(SQL, [Id, ChatId, Payload, UserId, Timestamp]).

get_message(MessageId) ->
    SQL = <<"SELECT * FROM message WHERE id = $1">>,
    query1(SQL, [MessageId]).

get_chat_messages(ChatId) ->
    SQL = <<"SELECT * FROM message where chat_id = $1">>,
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

add_participant(ChatId, UserId) ->
    SQL = <<"INSERT INTO participant (chat_id, user_id) VALUES ($1, $2)">>,
    query1(SQL, [ChatId, UserId]).

remove_participant(ChatId, UserId) ->
    SQL = <<"DELETE FROM participant WHERE chat_id = $1 AND user_id = $2">>,
    query1(SQL, [ChatId, UserId]).

get_participants(ChatId) ->
    SQL = <<"SELECT * FROM participant WHERE chat_id = $1">>,
    query(SQL, [ChatId]).

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
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL,
                                                                             Values]),
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