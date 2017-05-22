%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2017 12:40 AM
%%%-------------------------------------------------------------------
-module(test_message_handler).
-author("iguberman").

-behavior(satori_subscriber).

%% API

-export([
  process_messages/2,
  process_info/2,
  process_error/1,
  process_error/2]).

process_messages(Position, Messages) when is_list(Messages)->
  ct:print("Position ~p, num test messages: ~b", [Position, length(Messages)]),
  [ct:print("~p", [jsxn:decode(Message)]) || Message <- Messages],
  ok.

process_info(Position, {InfoType, InfoReason, MissedMessageCount}) ->
  ct:print("Test Subscription Info: Type ~p,  Reason ~p, MissedMessageCount ~b", [InfoType, InfoReason, MissedMessageCount]).

process_error({ErrorName, ErrorReason}) ->
  ct:print("Test Read Error ~p, Reason ~p", [ErrorName, ErrorReason]).

process_error(Position, {ErrorName, ErrorReason, MissedMessageCount}) ->
  ct:print("Test Subscription Error at position ~p: Name ~p, Reason ~p, MissedMessageCount ~b", [Position, ErrorName, ErrorReason, MissedMessageCount]).


