%%%-------------------------------------------------------------------
%% @doc satori_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('satori_client_sup').

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

  Children = [#{id => Type,
    start => {Type, start_link, []},
    restart => permanent,
    shutdown => 1000} || Type <- [satori_publisher, satori_subscriber, satori_key_value_store],
    application:get_env(satori_client, Type, false)],

  lager:info("Starting satori clients: ~p", [Children]),

  %% Restart Strategy
  RestartStrategy = {one_for_one, 4, 3600},

  {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
