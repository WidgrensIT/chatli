-module(chatli_json).

-export([
    encode/2,
    decode/2
]).

encode(Json, _) ->
    thoas:encode(Json).

decode(Json, _) ->
    {ok, Decode} = thoas:decode(Json),
    Decode.
