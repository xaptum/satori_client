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
  groups/0,
  init_per_group/1,
  end_per_group/1,
  init_per_suite/1,
  end_per_suite/1,
  test_publisher/1,
  test_subscriber/1]).

all() -> [{group, pub_sub}].

groups() -> [{pub_sub, [sequence], [test_subscriber, test_publisher]}].

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
      satori_client:start_subscriber(TestRole),
      satori_client:start_publisher(TestRole),
      [{test_role, TestRole}] ++ Config
  end.

end_per_suite(_Config) -> ok.

init_per_group(_Config) -> ok.
end_per_group(_Config) -> ok.

test_publisher(Config) ->
  TestRole = ?config(test_role, Config),
  ct:print("Sending messages!"),
    catch satori_publisher:publish(TestRole, "I'm not JSON!"),
  [satori_publisher:publish(TestRole, generate_message()) || I <- lists:seq(1,100)],
  timer:sleep(30000),
  ok.

test_subscriber(Config) ->
  TestRole = ?config(test_role, Config),
  satori_subscriber:subscribe(TestRole, TestRole,
    "select * where species = \"elephant\" and (heart-rate > 39 or heart-rate < 15) and (breath-rate < 9 and breath-rate > 15) and (temp < 36 or temp > 38)"),
  timer:sleep(1000),
  ok.

generate_message() ->
  HeartRate = crypto:rand_uniform(10, 45),
  BreathRate = crypto:rand_uniform(3, 20),
  Temp = crypto:rand_uniform(33, 41),
  Lat = crypto:rand_uniform(8, 73),
  Lon = crypto:rand_uniform(34, 80),
  AnimalId = crypto:rand_uniform(10000, 20000),
  Message = #{<<"species">> => <<"elephant">>,
    <<"heart-rate">> => HeartRate,
    <<"breath-rate">> => BreathRate,
    <<"temp">> => Temp,
    <<"location">> => #{<<"lat">> => Lat, <<"lon">> => Lon},
    <<"animal_id">> => AnimalId},
  jsxn:encode(Message).