%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2017 8:53 AM
%%%-------------------------------------------------------------------
-module(satori_SUITE).
-author("iguberman").

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0,
  init_per_suite/1,
  test_publisher/1,
  test_subscriber/1]).


all() -> [test_subscriber, test_publisher].

init_per_suite(Config) ->
  lager:start(),
  case os:getenv("SATORI_TEST_ROLE") of
    false ->
      ct:print("To run this test please register with Satori and set the following env vars:"),
      ct:print("SATORI_TEST_ROLE, SATORI_APPKEY, SATORI_ENDPOINT, and your_test_role_SATORI_ROLE_SECRET"),
      true = false,
      Config;
    TestRole ->
      application:ensure_all_started(satori_client),
      satori_client:start_publisher(TestRole),
      satori_client:start_subscriber(TestRole),
      [{test_role, TestRole}] ++ Config
  end.

test_publisher(Config) ->
  TestRole = ?config(test_role, Config),
  ct:print("Sending messages!"),
    catch satori_publisher:publish(TestRole, "I'm not JSON!"),
  satori_publisher:publish(TestRole, "{ \"temp\" : 75, \"msg\" : \"Neo says arf2!\"}"),
  satori_publisher:publish(TestRole, jsxn:encode(#{<<"temp">> => 100, <<"msg">> => <<"Zoomies2">>})),
  satori_publisher:publish(TestRole, jsxn:encode(#{<<"temp">> => 120, <<"msg">> => <<"Bezoomies3">>})),
  timer:sleep(20000),
  ok.

test_subscriber(Config) ->
  TestRole = ?config(test_role, Config),
  Channel = os:getenv("SATORI_CHANNEL"),
  satori_subscriber:subscribe(TestRole, Channel),
  ok.

generate_message() ->
  Temp = crypto:rand_uniform(10, 200),
  jsxn:encode(#{<<"temp">> => 100}).