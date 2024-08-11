-module(chatli_uuid).

-export([get_v4/0,
         get_v4_no_dash/1]).

get_v4() ->
    jhn_uuid:gen(v4, [binary]).

get_v4_no_dash(list) ->
    jhn_uuid:gen(v4, [binary, hex]).