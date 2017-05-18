%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2017 8:53 AM
%%%-------------------------------------------------------------------
-module(publisher_SUITE).
-author("iguberman").

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0,
  init_per_suite/1,
  test_publisher/1]).

all() -> [test_publisher].

init_per_suite(Config)->
  lager:start(),
  application:ensure_all_started(satori_client),
  Config.

test_publisher(Config)->
  ct:print("Sending messages!"),
  catch satori_publisher:publish("I'm not JSON!"),
  satori_publisher:publish("{ \"msg\" : \"Neo says arf2!\"}"),
  satori_publisher:publish(jsxn:encode(#{<<"msg">> => <<"Zoomies2">>})),
  satori_publisher:publish(jsxn:encode(#{<<"msg">> => <<"Bezoomies3">>})),
  timer:sleep(5000),
  ok.
