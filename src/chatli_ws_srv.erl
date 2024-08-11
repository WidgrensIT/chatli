-module(chatli_ws_srv).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    publish/2,
    callback/2,
    online/3,
    offline/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/1
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

publish(Topic, Body) ->
    gen_server:cast(?MODULE, {publish, Topic, Body}).

callback(UserId, Body) ->
    gen_server:cast(?MODULE, {callback, UserId, Body}).

online(User, Device, Socket) ->
    gen_server:call(?MODULE, {online, User, Device, Socket}).

offline(User, Device, Socket) ->
    gen_server:call(?MODULE, {offline, User, Device, Socket}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: term()}
    | {ok, State :: term(), Timeout :: timeout()}
    | {ok, State :: term(), hibernate}
    | {stop, Reason :: term()}
    | ignore.
init([]) ->
    process_flag(trap_exit, true),
    self() ! start,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
    {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()}
    | {reply, Reply :: term(), NewState :: term(), hibernate}
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
handle_call({online, User, Device, Socket}, _, State) ->
    true = ets:insert(online, {User, Device, Socket}),
    {reply, ok, State};
handle_call({offline, User, Device, Socket}, _, State) ->
    true = ets:delete_object(online, {User, Device, Socket}),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
handle_cast({callback, UserId, Body}, State) ->
    logger:debug("Got callback cast"),
    {ok, Callbacks} = chatli_db:get_user_callbacks(UserId),
    {ok, DecodedBody} = thoas:decode(Body),
    MergedBody = maps:merge(#{<<"to">> => UserId}, DecodedBody),
    Body2 = thoas:encode(MergedBody),
    [send_callback(Url, Body2) || #{url := Url} <- Callbacks],
    {noreply, State};
handle_cast({publish, Topic, Body}, State) ->
    {ok, Subscribers} = chatli_db:get_participants(Topic),
    [send(Body, online_sockets(UserId)) || #{user_id := UserId} <- Subscribers],
    [gen_server:cast(self(), {callback, UserId, Body}) || #{user_id := UserId} <- Subscribers],
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(start, State) ->
    ets:new(online, [named_table, bag]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: term()
) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: term(),
    Extra :: term()
) ->
    {ok, NewState :: term()}
    | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Status :: map()) -> Status :: map().
format_status(Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(_, []) ->
    ok;
send(Body, Sockets) ->
    [Socket ! Body || [Socket] <- Sockets].

online_sockets(User) ->
    ets:match(online, {User, '_', '$1'}).

send_callback(Url, Body) ->
    logger:debug("Send callback.. ~p", [Url]),
    logger:debug("body: ~p", [Body]),
    Opts = #{headers => #{'Content-Type' => <<"application/json">>}, close => true},
    Response = jhn_shttpc:post([Url], Body, Opts),
    logger:debug("response: ~p", [Response]),
    ok.
