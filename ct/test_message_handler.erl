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
  process_messages/1,
  process_info/1,
  process_error/1]).

process_messages(Messages) when is_list(Messages)->
  lager:info("Got ~b test messages:", [length(Messages)]),
  [lager:info("~p", [jsxn:decode(Message)]) || Message <- Messages],
  ok.

process_info({InfoType, InfoReason, MissedMessageCount}) ->
  lager:info("Test Subscription Info: Type ~p,  Reason ~p, MissedMessageCount ~b", [InfoType, InfoReason, MissedMessageCount]).

process_error({ErrorName, ErrorReason, MissedMessageCount}) ->
  lager:warning("Test Subsciprion Error: Name ~p, Reason ~p, MissedMessageCount ~b", [ErrorName, ErrorReason, MissedMessageCount]).


