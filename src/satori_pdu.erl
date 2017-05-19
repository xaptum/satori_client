%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2017 12:28 PM
%%%-------------------------------------------------------------------
-module(satori_pdu).
-author("iguberman").

-define(ROLE_SECRET, "role_secret").

%% API
-export([
  handshake_request/1,
  handshake_request/2,
  get_nonce_from_handshake_response/2,
  auth_request/3,
  auth_response/2,
  publish_request/3,
  write_request/3,
  get_position_from_publish_response/2,
  get_position_from_write_response/2,
  read_request/2,
  read_request/3,
  read_response/2,
  delete_request/2,
  delete_response/2,
  subscribe_request/2,
  view_subscribe_request/2,
  view_subscribe_request/3,
  parse_subscribe_response/2,
  parse_subscription/1,
  unsubscribe_request/2,
  parse_unsubscribe_response/2]).


%%====================================================================
%% HANDSHAKE PDU
%%====================================================================

handshake_request(Role) ->
  handshake_request(1, Role).

handshake_request(RequestId, Role) when is_list(Role) ->
  handshake_request(RequestId, list_to_binary(Role));
handshake_request(RequestId, Role) when is_binary(Role) ->
  Body = #{<<"method">> => <<"role_secret">>, <<"data">> => #{<<"role">> => Role}},
  Request = #{<<"action">> => <<"auth/handshake">>,
    <<"id">> => RequestId,
    <<"body">> => Body},
  {ok, jsxn:encode(Request)}.

get_nonce_from_handshake_response(RequestId, Resp) ->
  get_nonce_from_handshake_decoded_response(jsxn:decode(Resp), RequestId).

get_nonce_from_handshake_decoded_response(#{
  <<"action">> := <<"auth/handshake/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"data">> := #{<<"nonce">> := Nonce}}} = _Response, RequestId) ->
  lager:info("HANDSHAKE SUCCESS, Nonce ~p", [Nonce]),
  {ok, Nonce};
get_nonce_from_handshake_decoded_response(#{
  <<"action">> := <<"auth/handshake/error">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}} = _Response, RequestId) ->
  {error, {Error, Reason}}.

%%====================================================================
%% AUTHENTICATE PDU
%%====================================================================

auth_request(RequestId, RoleSecret, Nonce) ->
  Hash = calculate_hash(RoleSecret, Nonce),
  Body = #{<<"method">> => <<"role_secret">>, <<"credentials">> => #{<<"hash">> => Hash}},
  {ok, jsxn:encode(#{<<"action">> => <<"auth/authenticate">>, <<"id">> => RequestId, <<"body">> => Body})}.

auth_response(RequestId, Resp) ->
  auth_response_decoded(RequestId, jsxn:decode(Resp)).

auth_response_decoded(RequestId, #{
  <<"action">> := <<"auth/authenticate/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{}} = _Resp) ->
  ok;
auth_response_decoded(RequestId, #{
  <<"action">> := <<"auth/authenticate/error">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}} = _Resp) ->
  {error, {Error, Reason}}.


%%====================================================================
%% WRITE/PUBLISH PDU
%%====================================================================

publish_request(RequestId, Message, Channel) when is_list(Channel) ->
  publish_request(RequestId, Message, list_to_binary(Channel));
publish_request(RequestId, Message, Channel) when is_list(Message) ->
  publish_request(RequestId, list_to_binary(Message), Channel);
publish_request(RequestId, Message, Channel) when is_binary(Channel), is_binary(Message) ->
  publish_request(RequestId, Message, Channel, <<"rtm/publish">>).

write_request(RequestId, Message, Channel) ->
  publish_request(RequestId, Message, Channel, <<"rtm/write">>).

publish_request(RequestId, Message, Channel, Action) when is_list(Message) ->
  publish_request(RequestId, list_to_binary(Message), Channel, Action);
publish_request(RequestId, Message, Channel, Action) when is_binary(Message) ->
  case jsxn:is_json(Message) of
    true ->
      Req = #{<<"action">> => Action,
        <<"id">> => RequestId,
        <<"body">> => #{<<"channel">> => Channel, <<"message">> => Message}},
      {ok, jsxn:encode(Req)};
    false ->
      lager:error("Can't send message, it's not in valid JSON format: ~p", [Message]),
      {error, invalid_json_message}
  end.

get_position_from_publish_response(RequestId, Resp) ->
  {ok, Position} = get_position_from_publish_response_decoded(RequestId, jsxn:decode(Resp)),
  Position.

get_position_from_publish_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/publish/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"next">> := Position}}) ->
  {ok, Position};
get_position_from_publish_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/publish/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}}) ->
  {error, {Error, Reason}}.

get_position_from_write_response(RequestId, Resp) ->
  {ok, Position} = get_position_from_write_response_decoded(RequestId, jsxn:decode(Resp)),
  Position.

get_position_from_write_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/write/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"next">> := Position}}) ->
  {ok, Position};
get_position_from_write_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/write/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}}) ->
  {error, {Error, Reason}}.


%%====================================================================
%% SUBSCRIBE PDU
%%====================================================================

subscribe_request(RequestId, Channel) when is_list(Channel) ->
  subscribe_request(RequestId, list_to_binary(Channel));
subscribe_request(RequestId, Channel) when is_binary(Channel) ->
  RequestMap = subscribe_header(RequestId),
  {ok, jsxn:encode(RequestMap#{<<"body">> => #{<<"channel">> => Channel}})};
subscribe_request(RequestId, ParamsMap) when is_map(ParamsMap) ->
  true = body_parameters_valid(ParamsMap),
  RequestMap = subscribe_header(RequestId),
  Body = maps:from_list([valid_body_parameter(Key, ParamsMap) || Key <- maps:keys(ParamsMap)]),
  {ok, jsxn:encode(RequestMap#{<<"body">> => Body})}.  %% make sure channel = subscription_id

view_subscribe_request(RequestId, SubscriptionId, SQL) when is_integer(RequestId), is_list(SubscriptionId), is_list(SQL) ->
  RequestMap = subscribe_header(RequestId),
  {ok, jsxn:encode(RequestMap#{<<"body">> => #{<<"subscription_id">> => SubscriptionId, <<"filter">> => SQL}})}.

view_subscribe_request(RequestId, #{subscription_id := _SubscriptionId, filter := _SQL} = ParamsMap) ->
  RequestMap = subscribe_header(RequestId),
  Body = maps:from_list([valid_body_parameter(Key, ParamsMap) || Key <- maps:keys(ParamsMap)]),
  {ok, jsxn:encode(RequestMap#{<<"body">> => Body})}.  %% make sure channel = subscription_id


%% period is not a valid parameter in no-view subscription request
body_parameters_valid(#{period := _Period}) -> false;
%% if optional subscription_id field is present in no-view subscription request it should equal Channel
body_parameters_valid(#{channel := Channel, subscription_id := Channel}) -> true;
body_parameters_valid(#{channel := _Channel, subscription_id := _NotTheSameAsChannel}) -> false;
%% if none of the above and channel is present, it's valid
body_parameters_valid(#{channel := _Channel}) -> true;
body_parameters_valid(_Params) -> false.

valid_body_parameter(channel, #{channel := Channel}) when is_list(Channel) ->
  {<<"channel">>, list_to_binary(Channel)};
valid_body_parameter(channel, #{channel := Channel}) when is_binary(Channel) ->
  {<<"channel">>, Channel};
valid_body_parameter(subscription_id, #{subscription_id := SubscriptionId}) when is_list(SubscriptionId) ->
  {<<"subscription_id">>, list_to_binary(SubscriptionId)};
valid_body_parameter(subscription_id, #{subscription_id := SubscriptionId}) when is_binary(SubscriptionId) ->
  {<<"subscription_id">>, SubscriptionId};
valid_body_parameter(history, #{history := #{count := _Count, age := _Age}}) ->
  ok = {error, "Invalid history parameter: need either count or age, not both"};
valid_body_parameter(history, #{history := #{count := Count}}) when is_integer(Count) ->
  {<<"history">>, #{<<"count">> => Count}};
valid_body_parameter(history, #{history := #{age := Age}}) when is_integer(Age) ->
  {<<"history">>, #{<<"age">> => Age}};
valid_body_parameter(position, #{position := Position}) when is_list(Position) ->
  {<<"position">>, list_to_binary(Position)};
valid_body_parameter(position, #{position := Position}) when is_binary(Position) ->
  {<<"position">>, Position};
valid_body_parameter(filter, #{filter := SQL}) when is_list(SQL) ->
  {<<"filter">>, list_to_binary(SQL)};
valid_body_parameter(filter, #{filter := SQL}) when is_binary(SQL) ->
  {<<"filter">>, SQL};
valid_body_parameter(period, #{period := Period}) when is_integer(Period) ->
  {<<"period">>, Period};
valid_body_parameter(force, #{force := Force}) when is_boolean(Force) ->
  {<<"force">>, Force};
valid_body_parameter(fast_forward, #{fast_forward := FastForward}) when is_boolean(FastForward) ->
  {<<"fast_forward">>, FastForward};
valid_body_parameter(Parameter, ParamMap) when is_map(ParamMap) ->
  ok = {error, "Invalid parameter or value type", Parameter}.

subscribe_header(RequestId) ->
  #{<<"action">> => <<"rtm/subscribe">>, <<"id">> => RequestId}.

parse_subscribe_response(RequestId, Resp) ->
  parse_subscribe_response_decoded(jsxn:decode(Resp), RequestId).

parse_subscribe_response_decoded(RequestId,
    #{<<"action">> := <<"rtm/subscribe/ok">>,
      <<"id">> := RequestId,
      <<"body">> := #{<<"position">> := Position, <<"subscription_id">> := SubscriptionId}}) ->
  {ok, {Position, SubscriptionId}};
parse_subscribe_response_decoded(RequestId,
    #{<<"action">> := <<"rtm/subscribe/error">>,
      <<"id">> := RequestId,
      <<"body">> := #{<<"error">> := Error, <<"subscription_id">> := SubscriptionId} = RespBody}) ->
  Reason = get_reason(RespBody),
  {error, {{Error, Reason}, SubscriptionId}};
parse_subscribe_response_decoded(_RequestId, #{<<"action">> := _OtherAction} = OtherActionResponse) ->
  {other, OtherActionResponse}.

get_reason(#{<<"reason">> := Reason}) when is_binary(Reason) -> binary_to_list(Reason);
get_reason(#{<<"reason">> := Reason}) when is_list(Reason) -> Reason;
get_reason(_RespBody) -> <<"Reason not specified">>.

%%====================================================================
%% SUBSCRIPTION PDU
%%====================================================================

parse_subscription(Subscription) ->
  parse_subscription_decoded(jsxn:decode(Subscription)).

parse_subscription_decoded(
    #{<<"action">> := <<"rtm/subscription/data">>,
      <<"body">> := #{
    <<"position">> := Position,
      <<"messages">> := Messages,
      <<"subscription_id">> := SubscriptionId}}) ->
  {data, {Position, Messages, SubscriptionId}};
parse_subscription_decoded(#{
  <<"action">> := <<"rtm/subscription/info">>,
  <<"body">> := #{
    <<"info">> := InfoType,
    <<"reason">> := InfoReason,
    <<"position">> := Position,
    <<"subscription_id">> := SubscriptionId} = InfoBody}) ->
  MissedMessageCount = get_missed_message_count(InfoBody),
  {info, {Position, {InfoType, InfoReason, MissedMessageCount}, SubscriptionId}};
parse_subscription_decoded(#{
  <<"action">> := <<"rtm/subscription/error">>,
  <<"body">> := #{
  <<"error">> := ErrorName,
  <<"reason">> := ErrorReason,
  <<"position">> := Position,
  <<"subscription_id">> := SubscriptionId} = ErrorBody}) ->
  MissedMessageCount = get_missed_message_count(ErrorBody),
  {error, {Position, {ErrorName, ErrorReason, MissedMessageCount}, SubscriptionId}};
parse_subscription_decoded(#{<<"action">> := _NonSubscriptionAction} = OtherResponse) ->
  {other, OtherResponse}.

get_missed_message_count(#{<<"missed_message_count">> := MissedMessageCount}) when is_integer(MissedMessageCount) ->
  MissedMessageCount;
get_missed_message_count(_SubscriptionBody) -> 0.


%%====================================================================
%% UNSUBSCRIBE PDU
%%====================================================================

unsubscribe_request(RequestId, SubscriptionId) ->
  jsxn:encode(
    #{<<"action">> => <<"rtm/unsubscribe">>,
      <<"id">> => RequestId,
      <<"body">> => #{<<"subscription_id">> => SubscriptionId}}).

parse_unsubscribe_response(RequestId, Resp) ->
  parse_unsubscribe_response_decoded(RequestId, jsxn:decode(Resp)).

parse_unsubscribe_response_decoded(RequestId,
    #{<<"action">> := <<"rtm/unsubscribe/ok">>,
      <<"id">> := RequestId,
      <<"body">> := #{<<"position">> := Position, <<"subscription_id">> := SubscriptionId}
    }) ->
  {ok, {Position, SubscriptionId}};
parse_unsubscribe_response_decoded(RequestId,
    #{<<"action">> := <<"rtm/unsubscribe/error">>,
      <<"id">> := RequestId,
      <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason} = _ErrorBody}) ->
  {error, {Error, Reason}};
parse_unsubscribe_response_decoded(_RequestId, #{<<"action">> := _OtherAction} = OtherResponse) ->
  {other, OtherResponse}.


%%====================================================================
%% READ PDU
%%====================================================================

read_request(RequestId, Channel) when is_list(Channel) ->
  read_request(RequestId, list_to_binary(Channel));
read_request(RequestId, Channel) when is_binary(Channel) ->
  jsxn:encode(#{
    <<"action">> => <<"rtm/read">>,
    <<"id">> => RequestId,
    <<"body">> => #{<<"channel">> => Channel}}).

read_request(RequestId, Channel, Position) when is_list(Channel) ->
  read_request(RequestId, list_to_binary(Channel), Position);
read_request(RequestId, Channel, Position) when is_list(Position) ->
  read_request(RequestId, Channel, list_to_binary(Position));
read_request(RequestId, Channel, Position) when is_binary(Channel), is_binary(Position) ->
  jsxn:encode(#{<<"action">> => <<"rtm/read">>,
    <<"id">> => RequestId,
    <<"body">> => #{<<"channel">> => Channel, <<"position">> => Position}}).

read_response(RequestId, Resp) ->
  read_response_decoded(RequestId, jsxn:decode(Resp)).

read_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/read/ok">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"position">> := Position, <<"message">> := Message}}) ->
  {ok, {Position, Message}};
read_response_decoded(RequestId, #{
  <<"action">> := <<"rtm/read/error">>,
  <<"id">> := RequestId,
  <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}}) ->
  {ok, {Error, Reason}}.


%%====================================================================
%% DELETE PDU
%%====================================================================

delete_request(RequestId, Channel) when is_list(Channel)->
  delete_request(RequestId, list_to_binary(Channel));
delete_request(RequestId, Channel) when is_binary(Channel)->
  jsxn:encode(#{<<"action">> => <<"rtm/delete">>, <<"id">> => RequestId,
    <<"body">> => #{<<"channel">> => Channel}}).

delete_response(RequestId, Resp) ->
  delete_response_decoded(RequestId, jsxn:decode(Resp)).

delete_response_decoded(RequestId, #{
    <<"action">> := <<"rtm/delete/ok">>,
    <<"id">> := RequestId, <<"body">> := RespBody}) when is_map(RespBody) ->
  Position = get_position(RespBody),
  {ok, Position};
delete_response_decoded(RequestId,
    #{<<"action">> := <<"rtm/publish/error">>,
      <<"id">> := RequestId,
      <<"body">> := #{<<"error">> := Error, <<"reason">> := Reason}}) ->
  {error, {Error, Reason}}.

get_position(#{position := Position}) ->
  Position;
get_position(_RespBody) ->
  undefined.

%%====================================================================
%% UTILS
%%====================================================================

calculate_hash(RoleSecret, Nonce) ->
  HmacMd5 = crypto:hmac(md5, utf8(RoleSecret), utf8(Nonce)),
  base64:encode(HmacMd5).

utf8(String) when is_binary(String) ->
  utf8(binary_to_list(String));
utf8(String) when is_list(String) ->
  unicode:characters_to_list(String, utf8).

