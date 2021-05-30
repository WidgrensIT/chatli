-module(chatli_ws_client).

-export([init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3,
         ping_loop/1]).

init(#{req := #{bindings := #{deviceid := Device,
                              userid := User}}}) ->
     {ok, #{user => User,
            device => Device}}.

websocket_init(State) ->
    logger:info("Client online ~p", [State]),
    #{user := User,
        device := Device} = State,
    ok = chatli_ws_srv:online(User, Device, self()),
    Self = self(),
    spawn(fun() -> ping_loop(Self) end),
    {ok, State}.

websocket_handle(pong, State) ->
    {ok, State};
websocket_handle(Unexpected, State) ->
    logger:warning("UNEXPECTED: ~p State: ~p", [Unexpected, State]),
    {ok, State}.

websocket_info(ping, State) ->
    {reply, ping, State};
websocket_info(Payload, State) ->
    logger:info("Sending payload: ~p State: ~p", [Payload, State]),
    {reply, {text, Payload}, State}.

terminate(_, _, State)->
    logger:info("Client offline ~p", [State]),
    #{user := User,
        device := Device} = State,
    ok = chatli_ws_srv:offline(User, Device, self()).


ping_loop(Receiver) ->
    Receiver ! ping,
    receive
        stop ->
            ok
    after 5000 ->
            ping_loop(Receiver)
    end.
