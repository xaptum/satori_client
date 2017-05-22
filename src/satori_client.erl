%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2017 7:04 PM
%%%-------------------------------------------------------------------
-module(satori_client).
-author("iguberman").

%% API
-export([
  start_subscriber/1,
  start_publisher/1,
  start_key_value_store/1,
  registered_name/2,
  publisher_name/1,
  subscriber_name/1,
  key_value_store_name/1,
  websocket_name/2]).

start_publisher(Role)->
  supervisor:start_child(satori_publisher_sup, [Role]).

start_subscriber(Role)->
  supervisor:start_child(satori_subscriber_sup, [Role]).

start_key_value_store(Role)->
  supervisor:start_child(satori_key_value_store_sup, [Role]).

publisher_name(Role) -> registered_name(satori_publisher, Role).
subscriber_name(Role) -> registered_name(satori_subscriber, Role).
key_value_store_name(Role) -> registered_name(satori_key_value_store, Role).
websocket_name(Type, Role) ->
  list_to_atom("websocket_" ++ atom_to_list(registered_name(Type, Role))).

registered_name(Type, Role) when is_atom(Role)->
  registered_name(Type, atom_to_list(Role));
registered_name(Type, Role) when is_binary(Role)->
  registered_name(Type, binary_to_list(Role));
registered_name(Type, Role) when is_list(Role)->
  true = lists:member(Type, [satori_websocket, satori_publisher, satori_subscriber, satori_key_value_store]),
  list_to_atom(atom_to_list(Type) ++ "_" ++ Role).