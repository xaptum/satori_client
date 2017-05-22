%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2017 12:19 AM
%%%-------------------------------------------------------------------
-module(satori_dummy_message_handler).
-author("iguberman").
-behavior(satori_subscriber).

%% API
-export([
  process_messages/2,
  process_info/2,
  process_error/1,
  process_error/2]).

process_messages(Position, Messages) when is_list(Messages)->
  lager:info("Got messages at position ~p:", [Position]),
  [lager:info("~p", [jsxn:decode(Message)]) || Message <- Messages],
  ok.

process_info(Position, {InfoType, InfoReason, MissedMessageCount}) ->
 lager:info("Subscription Info at position ~p: Type ~p,  Reason ~p, MissedMessageCount ~b", [Position, InfoType, InfoReason, MissedMessageCount]).

process_error({Error, Reason})->
  lager:warning("Read error: ~p reason: ~p", [Error, Reason]).

process_error(Position, {ErrorName, ErrorReason, MissedMessageCount}) ->
  lager:warning("Subsciprion Error at position ~p: Name ~p, Reason ~p, MissedMessageCount ~b", [Position, ErrorName, ErrorReason, MissedMessageCount]).


