-module(chatli_uuid).

-export([get_v4/0,
         get_v4_no_dash/1]).

get_v4() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

get_v4_no_dash(list) ->
    uuid:uuid_to_string(uuid:get_v4(), nodash).