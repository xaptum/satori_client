%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2017 6:47 PM
%%%-------------------------------------------------------------------
-module(satori_subscriber_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  Children = [
    #{id => satori_subscriber,
    start => {satori_subscriber, start_link, []},
    restart => permanent,
    shutdown => 1000}],

  %% Restart Strategy
  RestartStrategy = {simple_one_for_one, 4, 3600},

  {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
