-module(chatli_db).

-export([
    create_message/1,
    get_message/2,
    get_chat_messages/1,
    create_chat/1,
    get_chat/1,
    get_all_chats/1,
    get_filtered_messages/2,
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
    create_attachment/4,
    get_attachment/2,
    query/2,
    query1/2
]).

create_message(#{
    <<"id">> := Id,
    <<"chat_id">> := ChatId,
    <<"payload">> := Payload,
    <<"sender">> := UserId,
    <<"sender_info">> := SenderInfoJson,
    <<"timestamp">> := Timestamp,
    <<"type">> := Type,
    <<"action">> := Action
}) ->
    SQL =
        <<
            "INSERT INTO message (id,\n"
            "                                  chat_id,\n"
            "                                  payload,\n"
            "                                  sender,\n"
            "                                  timestamp,\n"
            "                                  type,\n"
            "                                  action,\n"
            "                                  sender_info)\n"
            "             VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
        >>,
    query1(SQL, [Id, ChatId, Payload, UserId, Timestamp, Type, Action, SenderInfoJson]).

get_message(ChatId, MessageId) ->
    SQL =
        <<
            "SELECT id,\n"
            "                    chat_id,\n"
            "                    payload,\n"
            "                    sender,\n"
            "                    timestamp\n"
            "            FROM message\n"
            "            WHERE chat_id = $1 AND id = $2"
        >>,
    query1(SQL, [ChatId, MessageId]).

get_chat_messages(ChatId) ->
    SQL =
        <<
            "SELECT id,\n"
            "                    chat_id,\n"
            "                    payload,\n"
            "                    sender,\n"
            "                    timestamp\n"
            "            FROM message\n"
            "            WHERE chat_id = $1 \n"
            "            ORDER BY timestamp ASC"
        >>,
    query(SQL, [ChatId]).

get_filtered_messages(ChatId, QS) ->
    Where = <<" WHERE chat_id=$1 ">>,
    {Values, SqlWHERE} =
        case QS of
            #{
                <<"after">> := After,
                <<"before">> := Before
            } ->
                {
                    [ChatId, binary_to_integer(After), binary_to_integer(Before)],
                    <<Where/binary, " AND timestamp >= $2 AND timestamp <= $3 ">>
                };
            #{<<"after">> := After} ->
                {[ChatId, binary_to_integer(After)], <<Where/binary, " AND timestamp >= $2 ">>};
            #{<<"before">> := Before} ->
                {[ChatId, binary_to_integer(Before)], <<Where/binary, " AND timestamp <= $2 ">>};
            _ ->
                {[ChatId], Where}
        end,
    SQL =
        <<
            "SELECT id,\n"
            "                    chat_id,\n"
            "                    payload,\n"
            "                    sender,\n"
            "                    sender_info,\n"
            "                    timestamp\n"
            "            FROM message",
            SqlWHERE/binary,
            "ORDER BY timestamp ASC"
        >>,
    query(SQL, Values).

create_chat(#{
    <<"id">> := Id,
    <<"name">> := Name,
    <<"description">> := Description,
    <<"type">> := Type
}) ->
    SQL = <<"INSERT INTO chat (id, name, description, type) VALUES ($1, $2, $3, $4)">>,
    query1(SQL, [Id, Name, Description, Type]).

get_chat(ChatId) ->
    SQL = <<"SELECT * FROM chat WHERE id = $1">>,
    query1(SQL, [ChatId]).

get_dm_chat(User1, User2) ->
    SQL =
        <<
            "SELECT chat.*\n"
            "             FROM chat\n"
            "             INNER JOIN participant AS p1 ON p1.user_id = $1 AND p1.chat_id = chat.id\n"
            "             INNER JOIN participant AS p2 ON p2.user_id = $2 AND p2.chat_id = chat.id\n"
            "             WHERE chat.type = '1to1' LIMIT 1"
        >>,
    query1(SQL, [User1, User2]).

get_all_chats(UserId) ->
    SQL =
        <<
            "SELECT chat.*\n"
            "             FROM chat\n"
            "             INNER JOIN participant\n"
            "                ON participant.user_id = $1 AND\n"
            "                   participant.chat_id = chat.id"
        >>,
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
    SQL =
        <<
            "SELECT chatli_user.id,\n"
            "                    chatli_user.username,\n"
            "                    chatli_user.email\n"
            "             FROM participant\n"
            "             INNER JOIN chatli_user ON chatli_user.id = participant.user_id\n"
            "             WHERE participant.chat_id = $1 AND participant.user_id != $2"
        >>,
    query(SQL, [ChatId, UserId]).

get_participants(ChatId) ->
    SQL = <<"SELECT user_id FROM participant WHERE chat_id = $1">>,
    query(SQL, [ChatId]).

create_callback(CallbackId, UserId, Url) ->
    SQL =
        <<
            "INSERT INTO callback\n"
            "                (\n"
            "                  id,\n"
            "                  user_id,\n"
            "                  url\n"
            "                )\n"
            "             VALUES\n"
            "               (\n"
            "                 $1,\n"
            "                 $2,\n"
            "                 $3\n"
            "               )"
        >>,
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

create_attachment(AttachmentId, ChatId, Mime, ByteSize) ->
    SQL = <<"INSERT INTO attachment (id, chat_id, mime, length) VALUES ($1, $2, $3, $4)">>,
    query1(SQL, [AttachmentId, ChatId, Mime, ByteSize]).

get_attachment(AttachmentId, ChatId) ->
    SQL = <<"SELECT * FROM attachment WHERE id = $1 AND chat_id = $2">>,
    query1(SQL, [AttachmentId, ChatId]).

% Expect 1 result
query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{
            command := insert,
            num_rows := 1
        } ->
            ok;
        #{
            command := select,
            rows := []
        } ->
            undefined;
        #{
            command := select,
            rows := [Row]
        } ->
            {ok, Row};
        #{
            command := update,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := delete,
            num_rows := 1
        } ->
            ok;
        #{command := delete} ->
            undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{
            command := insert,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := select,
            rows := Rows
        } ->
            {ok, Rows};
        #{
            command := update,
            num_rows := Num
        } ->
            {ok, Num};
        #{
            command := delete,
            num_rows := Num
        } ->
            {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.
