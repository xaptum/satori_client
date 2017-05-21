%%%-------------------------------------------------------------------
%% @doc satori_client public API
%% @end
%%%-------------------------------------------------------------------

-module('satori_client_app').

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  lager:info("Starting satori client sup"),
  'satori_client_sup':start_link().


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
