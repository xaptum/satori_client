%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2017 12:19 AM
%%%-------------------------------------------------------------------
-module(dummy_message_handler).
-author("iguberman").
-behavior(satori_subscriber).

%% API
-export([
  process_messages/1,
  process_info/1,
  process_error/1]).

process_messages(Messages) when is_list(Messages)->
  lager:info("Got messages:"),
  [lager:info("~p", [jsxn:decode(Message)]) || Message <- Messages],
  ok.

process_info({InfoType, InfoReason, MissedMessageCount}) ->
 lager:info("Subscription Info: Type ~p,  Reason ~p, MissedMessageCount ~b", [InfoType, InfoReason, MissedMessageCount]).

process_error({ErrorName, ErrorReason, MissedMessageCount}) ->
  lager:warning("Subsciprion Error: Name ~p, Reason ~p, MissedMessageCount ~b", [ErrorName, ErrorReason, MissedMessageCount]).


