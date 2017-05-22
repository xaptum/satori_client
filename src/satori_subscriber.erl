%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2017 10:42 PM
%%%-------------------------------------------------------------------
-module(satori_subscriber).
-author("iguberman").
-behavior(gen_server).

-callback process_messages(Position :: binary(), Msg :: list()) -> Void :: any().
-callback process_info(Position :: binary(), Info :: tuple()) -> Void :: any().
-callback process_error(Position :: binary(), Error :: tuple()) -> Void :: any().
-callback process_error(Error :: tuple()) -> Void :: any().

-record(state, {message_handler, websocket_pid, status, channel, sub_id, position, request_id}).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% API
-export([
  subscribe/2,
  subscribe/3,
  unsubscribe/2,
  read/3,
  start_link/1
]).

subscribe(Role, Channel) ->
  gen_server:cast(satori_client:subscriber_name(Role), {subscribe, Channel}).

subscribe(Role, SubscriptionId, SQL) ->
  gen_server:cast(satori_client:subscriber_name(Role), {subscribe, {SubscriptionId, SQL}}).

unsubscribe(Role, SubscriptionId) ->
  gen_server:cast(satori_client:subscriber_name(Role), {unsubscribe, SubscriptionId}).

read(Role, Position, Channel)->
  gen_server:cast(satori_client:subscriber_name(Role), {read, {Position, Channel}}).

start_link(Role) ->
  gen_server:start_link({local, satori_client:subscriber_name(Role)}, ?MODULE, [Role], []).

init([Role]) ->
  {ok, WebsocketPid} = supervisor:start_child(satori_websocket_sup, [satori_subscriber, Role]),
  {ok, MessageHandler} = application:get_env(message_handler),
  {ok, #state{message_handler = MessageHandler, websocket_pid = WebsocketPid, status = connecting, request_id = 3}}.

handle_call(_Request, _From, State) ->
  lager:warning("Don't know how to handle_call(~p, ~p, ~p)", [_Request, _From, State]),
  {reply, unsupported, State}.

handle_cast(ready, State) ->
  {noreply, State#state{status = ready}};
handle_cast({subscribe, {SubscriptionId, SQL}},
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, SubscribeReq} = satori_pdu:view_subscribe_request(RequestId, SubscriptionId, SQL),
  lager:info("SUBSCRIBING to view ~p with SQL ~p: ~p", [SubscriptionId, SQL, SubscribeReq]),
  satori_websocket:send_request(WebsocketPid, SubscribeReq),
  {noreply, State#state{status = subscribing, sub_id = SubscriptionId}};
handle_cast({subscribe, Channel},
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, SubscribeReq} = satori_pdu:subscribe_request(RequestId, Channel),
  lager:info("SUBSCRIBING to ~p: ~p", [Channel, SubscribeReq]),
  satori_websocket:send_request(WebsocketPid, SubscribeReq),
  {noreply, State#state{status = subscribing, sub_id = list_to_binary(Channel), channel = list_to_binary(Channel)}};
handle_cast({subscribe, _SubscribeInfo} = Req, #state{status = connecting} = State) ->
  lager:warning("Got subscribe request while still connecting, retrying in 200ms"),
  timer:sleep(200),
  gen_server:cast(self(), Req),
  {noreply, State};
handle_cast({unsubscribe, SubscriptionId} = Req,
    #state{websocket_pid = WebsocketPid, status = subscribed, request_id = RequestId, sub_id = SubscriptionId} = State) ->
  {ok, UnsubscribeReq} = satori_pdu:unsubscribe_request(RequestId, SubscriptionId),
  lager:warning("UNSUBSCRIBING ~p: ~p", [SubscriptionId, UnsubscribeReq]),
  satori_websocket:send_request(WebsocketPid, UnsubscribeReq),
  {noreply, State#state{status = unsubscribing}};
handle_cast({read, {Position, Channel}}, #state{websocket_pid = WebsocketPid, status = subscriber, request_id = RequestId} = State)->
  {ok, ReadReq} = satori_pdu:read_request(RequestId, Channel, Position),
  lager:info("READ request for channel ~p at position ~p", [Channel, Position]),
  satori_websocket:send_request(WebsocketPid, ReadReq),
  {noreply, State};
%% we don't expect any other type of message from satori, but publish response
handle_cast({on_message, SubscribeResponse}, #state{status = subscribing, sub_id = SubscriptionId, request_id = RequestId} = State) ->
  case satori_pdu:parse_subscribe_response(RequestId, SubscribeResponse) of
    {ok, {Position, SubscriptionId}} ->
      lager:info("SUBSCRIBE SUCCESS at ~p for subscription_id ~p", [Position, SubscriptionId]),
      {noreply, State#state{position = Position, request_id = RequestId + 1, status = subscribed}};
    {ok, {Position, OtherSubscriptionId}} ->
      lager:info("SUBSCRIBE SUCCESS at ~p for OTHER! subscription_id ~p", [Position, OtherSubscriptionId]),
      {noreply, State#state{position = Position, sub_id = OtherSubscriptionId, request_id = RequestId + 1, status = subscribed}};
    {error, {{Error, Reason}, SubscriptionId}} ->
      lager:error("Failed to parse subscribe response ~p, error: ~p", [SubscribeResponse, Error]),
      {stop, Reason, State};
    {other, OtherActionResponse} ->
      lager:error("Unexpected Response ~p", [OtherActionResponse]),
      {noreply, State}
  end;
handle_cast({on_message, SubscriptionOrResponse}, #state{message_handler = MessageHandler, status = subscribed, sub_id = SubscriptionId, request_id = RequestId} = State) ->
  lager:info("Subscriber got message: ~p", [SubscriptionOrResponse]),
  case satori_pdu:parse_channel_data(SubscriptionOrResponse) of
    {data, {Position, Messages, SubscriptionId}} ->
      MessageHandler:process_messages(Position, Messages);
    {info, {Position, {_InfoType, _InfoReason, _MissedMessageCount} = Info, SubscriptionId}} ->
      MessageHandler:process_info(Position, Info);
    {error, {Position, {_ErrorName, _ErrorReason, _MissedMessageCount} = Error, SubscriptionId}} ->
      MessageHandler:process_error(Position, Error);
    {other, SubscriptionOrResponse} ->
      case satori_pdu:read_response(RequestId, SubscriptionOrResponse) of
        {ok, {Position, Message}} -> MessageHandler:process_messages(Position, [Message]);
        {error, {Error, Reason}} -> MessageHandler:process_error({Error, Reason});
        {other, SubscriptionOrResponse } -> lager:error("Unexpected subscription or read response: ~p", [SubscriptionOrResponse])
      end
  end,
  {noreply, State}.


handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

