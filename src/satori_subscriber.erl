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

-callback process_messages(Msg :: list()) -> Void :: any().
-callback process_info(Info :: tuple()) -> Void :: any().
-callback process_error(Msg :: tuple()) -> Void :: any().

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
  subscribe/1,
  subscribe/2,
  unsubscribe/1,
  start_link/0,
  start_link/1
]).


subscribe(Channel) ->
  gen_server:cast(?MODULE, {subscribe, Channel}).

subscribe(SubscriptionId, SQL) ->
  gen_server:cast(?MODULE, {subscribe, {SubscriptionId, SQL}}).

unsubscribe(SubscriptionId) ->
  gen_server:cast(?MODULE, {unsubscribe, SubscriptionId}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(SubscriberId) ->
  gen_server:start_link({local, SubscriberId}, ?MODULE, [], []).

init([]) ->
  {ok, SatoriClientPid} = satori_client:start_link(self()),
  {ok, MessageHandler} = application:get_env(message_handler),
  {ok, #state{message_handler = MessageHandler, websocket_pid = SatoriClientPid, status = connecting, request_id = 3}}.

handle_call(_Request, _From, State) ->
  lager:warning("Don't know how to handle_call(~p, ~p, ~p)", [_Request, _From, State]),
  {reply, unsupported, State}.

handle_cast(ready, State) ->
  {noreply, State#state{status = ready}};
handle_cast({subscribe, Channel},
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, SubscribeReq} = satori_pdu:subscribe_request(RequestId, Channel),
  lager:info("SUBSCRIBING to ~p: ~p", [Channel, SubscribeReq]),
  satori_client:send_request(WebsocketPid, SubscribeReq),
  {noreply, State#state{status = subscribing, sub_id = Channel, channel = Channel}};
handle_cast({subscribe, {SubscriptionId, SQL}},
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, SubscribeReq} = satori_pdu:view_subscribe_request(RequestId, SubscriptionId, SQL),
  lager:info("SUBSCRIBING to view ~p with SQL ~p: ~p", [SubscriptionId, SQL, SubscribeReq]),
  satori_client:send_request(WebsocketPid, SubscribeReq),
  {noreply, State#state{status = subscribing, sub_id = SubscriptionId}};
handle_cast({subscribe, _SubscribeInfo} = Req, #state{status = connecting} = State) ->
  lager:warning("Got subscribe request while still connecting, retrying in 500ms"),
  timer:sleep(500),
  gen_server:cast(self(), Req),
  {noreply, State};
handle_cast({unsubscribe, SubscriptionId} = Req,
    #state{websocket_pid = WebsocketPid, status = subscribed, request_id = RequestId, sub_id = SubscriptionId} = State) ->
  {ok, UnsubscribeReq} = satori_pdu:unsubscribe_request(RequestId, SubscriptionId),
  lager:warning("UNSUBSCRIBING ~p: ~p", [SubscriptionId, UnsubscribeReq]),
  satori_client:send_request(WebsocketPid, UnsubscribeReq),
  {noreply, State#state{status = unsubscribing}};
%% we don't expect any other type of message from satori, but publish response
handle_cast({on_message, SubscribeResponse}, #state{status = subscribing, sub_id = SubscriptionId, request_id = RequestId} = State) ->
  case satori_pdu:parse_subscribe_response(RequestId, SubscribeResponse) of
    {ok, {Position, SubscriptionId}} ->
      lager:info("SUBSCRIBE SUCCESS at ~p for subscription_id ~p", [Position, SubscriptionId]),
      {noreply, State#state{position = Position, request_id = RequestId + 1, status = subscribed}};
    {error, {{Error, Reason}, SubscriptionId}} ->
      lager:error("Failed to parse subscribe response ~p, error: ~p", [SubscribeResponse, Error]),
      {stop, Reason, State};
    {other, OtherActionResponse} ->
      lager:error("Unexpected Response ~p", [OtherActionResponse]),
      {noreply, State}
  end;
handle_cast({on_message, Subscription}, #state{message_handler = MessageHandler, status = subscribed, sub_id = SubscriptionId} = State) ->
  case satori_pdu:parse_subscription(Subscription) of
    {data, {_Position, Messages, SubscriptionId}} ->
      MessageHandler:process_messages(Messages);
    {info, {_Position, {_InfoType, _InfoReason, _MissedMessageCount} = Info, SubscriptionId}} ->
      MessageHandler:process_info(Info);
    {error, {_Position, {_ErrorName, _ErrorReason, _MissedMessageCount} = Error, SubscriptionId}} ->
      MessageHandler:process_error(Error);
    {other, OtherResponse} -> lager:error("Unexpected subscription: ~p", [OtherResponse])
  end,
  {noreply, State}.


handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

