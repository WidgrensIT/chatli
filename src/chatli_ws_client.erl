-module(chatli_ws_client).

-export([init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

init(#{req := #{bindings := #{deviceid := Device,
                              userid := User}}}) ->
     {ok, #{user => User,
            device => Device}}.

websocket_init(State) ->
    logger:info("Client online ~p", [State]),
    #{user := User,
        device := Device} = State,
    ok = chatli_ws_srv:online(User, Device, self()),
    {ok, State}.

websocket_handle(ping, State) ->
    logger:info("Ping from client ~p", [State]),
    {ok, State};
websocket_handle(Unexpected, State) ->
    logger:warning("UNEXPECTED: ~p State: ~p", [Unexpected, State]),
    {ok, State}.

websocket_info(Payload, State) ->
    logger:info("Sending payload: ~p State: ~p", [Payload, State]),
    {reply, {text, Payload}, State}.

terminate(_, _, State)->
    logger:info("Client offline ~p", [State]),
    #{user := User,
        device := Device} = State,
    ok = chatli_ws_srv:offline(User, Device, self()).