-module(chatli_callback).

-export([send/1]).

-spec send(map()) -> ok.
send(Object) ->
    Encoded = json:encode(Object, [maps, binary]),
    Opts = #{headers => #{'Content-Type' => <<"application/json">>},
                                                       close => true},
    Result = shttpc:post([<<"http://localhost:8090/receiver">>],
                                         Encoded,
                                         Opts),
    logger:debug("callback result: ~p", [Result]),
    ok.