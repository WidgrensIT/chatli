%%%-------------------------------------------------------------------
%% @doc chatli top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chatli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [child(chatli_ws_srv, worker)]} }.

%%====================================================================
%% Internal functions
%%====================================================================

child(Module, Type) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => Type,
      modules => [Module]}.