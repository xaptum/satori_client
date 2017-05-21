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

-define(SUPERVISOR(Type),
  #{id => Type,
  type => supervisor,
  start => {Type, start_link, []},
  restart => permanent,
  shutdown => 1000}).

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

  Children = [
    ?SUPERVISOR(satori_websocket_sup),
    ?SUPERVISOR(satori_publisher_sup),
    ?SUPERVISOR(satori_subscriber_sup),
    ?SUPERVISOR(satori_key_value_store_sup)],

  %% Restart Strategy
  RestartStrategy = {one_for_one, 4, 3600},

  {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
