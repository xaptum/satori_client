%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2017 10:42 PM
%%%-------------------------------------------------------------------
-module(satori_publisher).
-author("iguberman").
-behavior(gen_server).

-record(state, {websocket_pid, status, channel, sub_id, position, request_id}).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% API
-export([
  publish/1,
  publish/2,
  start_link/0,
  start_link/1
]).

publish(PublisherPid, Message) ->
  gen_server:cast(PublisherPid, {publish, Message}).

publish(Message) ->
  gen_server:cast(?MODULE, {publish, Message}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(PublisherId) ->
  gen_server:start_link({local, PublisherId}, ?MODULE, [], []).

init([]) ->
  {ok, SatoriClientPid} = satori_client:start_link(self()),
  Channel = os:getenv("SATORI_CHANNEL"),
  {ok, #state{websocket_pid = SatoriClientPid, channel = Channel, status = connecting, request_id = 3}}.

handle_call(_Request, _From, State) ->
  lager:warning("Don't know how to handle_call(~p, ~p, ~p)", [_Request, _From, State]),
  {reply, unsupported, State}.

handle_cast(ready, State) ->
  {noreply, State#state{status = ready}};
handle_cast({publish, Message}, #state{websocket_pid = WebsocketPid, channel = Channel, status = ready, request_id = RequestId} = State) ->
  case satori_pdu:publish_request(RequestId, Message, Channel) of
    {ok, PublishReq} ->
      lager:info("PUBLISHING: ~p", [PublishReq]),
      satori_client:send_request(WebsocketPid, PublishReq),
      {noreply, State#state{status = publishing}};
    {error, Error} ->
      lager:error("Error publishing message ~p: ~p", [Message, Error]),
      {noreply, State}
  end;
handle_cast({publish, _Message} = Req, #state{status = connecting} = State) ->
  lager:warning("Got publish request while still connecting, retrying in 500ms"),
  timer:sleep(500),
  gen_server:cast(self(), Req),
  {noreply, State};
handle_cast({publish, _Message} = Req, #state{status = publishing} = State) ->
  lager:warning("Got publish request while waiting on publish response, retrying in 200ms"),
  timer:sleep(200),
  gen_server:cast(self(), Req),
  {noreply, State};
%% we don't expect any other type of message from satori, but publish response
handle_cast({on_message, PublishResponse}, #state{status = publishing, request_id = RequestId} = State) ->
  Position = satori_pdu:get_position_from_publish_response(RequestId, PublishResponse),
  lager:info("PUBLISH SUCCESS at position ~p", [Position]),
  {noreply, State#state{position = Position, request_id = RequestId + 1, status = ready}}.

handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

