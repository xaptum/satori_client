%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2017 10:42 PM
%%%-------------------------------------------------------------------
-module(satori_publisher).
-author("iguberman").
-behavior(gen_server).

-record(state, {websocket_pid, status, channel, sub_id, position, num_requests}).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% API
-export([
  publish/2,
  start_link/1
]).

publish(Role, Message) ->
  gen_server:cast(satori_client:publisher_name(Role), {publish, Message}).

start_link(Role) ->
  gen_server:start_link({local, satori_client:publisher_name(Role)}, ?MODULE, [Role], []).

init([Role]) ->
  {ok, WebsocketPid} = supervisor:start_child(satori_websocket_sup, [satori_publisher, Role]),
  {ok, #state{websocket_pid = WebsocketPid, channel = Role, status = connecting, num_requests = 0}}.

handle_call(_Request, _From, State) ->
  lager:warning("Don't know how to handle_call(~p, ~p, ~p)", [_Request, _From, State]),
  {reply, unsupported, State}.

handle_cast(ready, State) ->
  {noreply, State#state{status = ready}};
handle_cast({publish, Message}, #state{websocket_pid = WebsocketPid, channel = Channel, status = ready, num_requests = NumRequests} = State) ->
  case satori_pdu:publish_request(current_time(), Message, Channel) of
    {ok, PublishReq} ->
      lager:info("PUBLISHING: ~p", [PublishReq]),
      satori_websocket:send_request(WebsocketPid, PublishReq),
      {noreply, State#state{num_requests = NumRequests + 1}};
    {error, Error} ->
      lager:error("Error publishing message ~p: ~p", [Message, Error]),
      {noreply, State}
  end;
handle_cast({publish, _Message} = Req, #state{status = connecting} = State) ->
  lager:warning("Got publish request while still connecting, retrying in 500ms"),
  timer:sleep(500),
  gen_server:cast(self(), Req),
  {noreply, State};
%%handle_cast({publish, _Message} = Req, #state{status = publishing} = State) ->
%%  lager:warning("Got publish request while waiting on publish response, retrying in 200ms"),
%%  timer:sleep(200),
%%  gen_server:cast(self(), Req),
%%  {noreply, State};
%% we don't expect any other type of message from satori, but publish response
handle_cast({on_message, PublishResponse}, #state{status = ready, num_requests = NumRequests} = State) when NumRequests > 0 ->
  Position = satori_pdu:get_position_from_publish_response(PublishResponse),
  lager:info("PUBLISH SUCCESS at position ~p", [Position]),
  {noreply, State#state{position = Position, num_requests = NumRequests - 1}}.

handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


current_time() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.

