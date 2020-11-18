-module(chatli_callback).

-export([send/1]).

send(Object) ->
    shttpc:post([<<"http://localhost:8090/receiver">>],
                json:encode(Object, [maps, binary]),
                #{headers => #{'Content-Type' => <<"application/json">>}, close => true}).