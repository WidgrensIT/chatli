-module(chatli_user_db).

-export([create/1,
         get/1,
         get_login/2,
         find/2,
         delete/1,
         get_all/0,
         get_all_other/1]).

create(#{id := Id,
              username := Username,
              phone_number := PhoneNumber,
              email := Email,
              password := Password}) ->
    SQL = <<"INSERT INTO chatli_user (id, username, phone_number, email, password) VALUES ($1, $2, $3, $4, $5)">>,
    chatli_db:query1(SQL, [Id, Username, PhoneNumber, Email, Password]).

get(UserId) ->
    SQL = <<"SELECT id, username, phone_number, email FROM chatli_user WHERE id = $1">>,
    chatli_db:query1(SQL, [UserId]).

get_login(Username, Password) ->
    SQL = <<"SELECT * FROM chatli_user WHERE username = $1 AND password = $2">>,
    chatli_db:query1(SQL, [Username, Password]).

find(Type, Value) ->
    SQL = <<"SELECT * FROM chatli_user WHERE ">>,
    WhereSQL = case Type of
                   <<"email">> ->
                        <<"email = $1">>;
                   <<"phone_number">> ->
                        <<"phone_number = $1">>
               end,
    chatli_db:query1(<<SQL/binary, WhereSQL/binary>>, [Value]).

delete(UserId) ->
    SQL = <<"DELETE FROM chatli_user WHERE id = $1">>,
    chatli_db:query1(SQL, [UserId]).

get_all() ->
    SQL = <<"SELECT id, avatar, email, phone_number, username FROM chatli_user">>,
    chatli_db:query(SQL, []).

get_all_other(UserId) ->
    SQL = <<"SELECT id, avatar, email, phone_number, username FROM chatli_user WHERE id != $1">>,
    chatli_db:query(SQL, [UserId]).