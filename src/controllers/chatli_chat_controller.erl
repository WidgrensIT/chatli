-module(chatli_chat_controller).
-export([
         message/1,
         chat/1,
         manage_chat/1,
         participants/1,
         manage_participants/1
        ]).

message(#{req := #{method := <<"POST">>},
          json := Json}) ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    logger:debug("json ~p", [Json]),
    {json, 201, #{}, #{id => Id}}.

chat(#{req := #{method := <<"GET">>,
                bindings := #{userid := UserId}}}) ->
    case chatli_db:get_all_chats(UserId) of
        {ok, Chats} ->
            {json, 200, #{}, Chats};
        Error ->
            logger:warning("chat error: ~p", [Error]),
            {json, 200, #{}, []}
    end;
chat(#{req := #{method := <<"POST">>},
       json := Json}) ->
    Id = uuid:uuid_to_string(uuid:get_v4()),
    Object = maps:merge(#{<<"id">> => Id}, Json),
    case chatli_db:create_chat(Object) of
        {ok, _} ->
            {json, 201, #{}, Object};
        Error ->
            logger:warning("chat error: ~p", [Error]),
            {status, 500}
    end.

manage_chat(#{req := #{ method := <<"GET">>,
                        bindings := #{chatid := ChatId}}}) ->
    case chatli_db:get_chat(ChatId) of
        {ok, Chat} ->
            {json, 200, #{}, Chat};
        Error ->
            logger:warning("chat error: ~p", [Error]),
            {status, 500}
    end;
manage_chat(#{req := #{ method := <<"DELETE">>,
                        bindings := #{chatid := ChatId}}}) ->
    logger:debug("chatid: ~p", [ChatId]),
    {status, 200}.

participants(#{ req := #{method := <<"GET">>,
                         bindings := #{chatid := ChatId}}}) ->
    case chatli_db:get_participants(ChatId) of
        {ok, Participants} ->
            {json, 200, #{}, #{id => ChatId,
                               participants => Participants}};
        Error ->
            logger:warning("participants error: ~p", [Error]),
            {status, 500}
    end;
participants(#{ req := #{method := <<"POST">>,
                         bindings := #{chatid := ChatId}},
                json := Json}) ->
    #{<<"id">> := UserId} = Json,
    case chatli_db:add_participant(ChatId, UserId) of
        {ok, _} ->
            {status, 201};
        Error ->
            logger:warning("participants error: ~p", [Error]),
            {status, 500}
    end.

manage_participants(#{req := #{ method := <<"DELETE">>},
                                bindigns := #{chatid := ChatId,
                                              participantid := ParticipantId}}) ->
    logger:debug("chatid: ~p participantid: ~p", [ChatId, ParticipantId]),
    chatli_db:remove_participant(ChatId, ParticipantId),
    {status, 200}.