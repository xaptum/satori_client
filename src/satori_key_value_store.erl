%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2017 1:14 PM
%%%-------------------------------------------------------------------
-module(satori_key_value_store).
-author("iguberman").
-behavior(gen_server).

-record(state, {message_handler, websocket_pid, status, channel, request_id}).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% API
-export([
  write/3,
  read/2,
  delete/2,
  start_link/1
]).

write(Role, Channel, Message) ->
  gen_server:cast(satori_client:key_value_store_name(Role), {write, {Channel, Message}}).

read(Role, Channel)->
  gen_server:cast(satori_client:key_value_store_name(Role), {read, Channel}).

delete(Role, Channel)->
  gen_server:cast(satori_client:key_value_store_name(Role), {delete, Channel}).


start_link(Role) ->
  gen_server:start_link({local, satori_client:key_value_store_name(Role)}, ?MODULE, [Role], []).

init([Role]) ->
  {ok, WebsocketPid} = supervisor:start_child(satori_websocket_sup, [satori_publisher, Role]),
  {ok, #state{websocket_pid = WebsocketPid, channel = Role, status = connecting, request_id = 3}}.

handle_call(_Request, _From, State) ->
  lager:warning("Don't know how to handle_call(~p, ~p, ~p)", [_Request, _From, State]),
  {reply, unsupported, State}.

%% CAST FROM WEBSOCKET
handle_cast(ready, State) ->
  {noreply, State#state{status = ready}};

%% REQUESTS
handle_cast({read, Channel} = Req,
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, ReadReq} = satori_pdu:read_request(RequestId, Channel),
  satori_websocket:send_request(WebsocketPid, ReadReq),
  {noreply, State#state{status = reading}};
handle_cast({write, {Channel, Message}} = Req,
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, ReadReq} = satori_pdu:write_request(RequestId, Message, Channel),
  satori_websocket:send_request(WebsocketPid, ReadReq),
  {noreply, State#state{status = writing}};
handle_cast({delete, Channel} = Req,
    #state{websocket_pid = WebsocketPid, status = ready, request_id = RequestId} = State) ->
  {ok, DeleteReq} = satori_pdu:delete_request(RequestId, Channel),
  satori_websocket:send_request(WebsocketPid, DeleteReq),
  {noreply, State#state{status = reading}};

%% RESPONSES
handle_cast({on_message, ReadResponse}, #state{message_handler = MessageHandler, status = reading, request_id = RequestId} = State) ->
  case satori_pdu:read_response(RequestId, ReadResponse) of
    {ok, {Position, Message}} -> MessageHandler:process_messages(Position, [Message]),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {error, {Error, Reason}} -> MessageHandler:process_error({Error, Reason}),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {other, ReadResponse} -> lager:error("Unexpected response ~p. Continue waiting for read response...", [ReadResponse]),
      {noreply, State}
  end;
handle_cast({on_message, WriteResponse}, #state{status = writing, request_id = RequestId} = State) ->
  case satori_pdu:write_response(RequestId, WriteResponse) of
    {ok, Position} -> lager:info("WRITE SUCCESS at position ~p", [Position]),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {error, {Error, Reason}} ->
      lager:warning("WRITE ERROR: ~p, REASON: ~p", [Error, Reason]),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {other, WriteResponse} -> lager:error("Unexpected response ~p. Continue waiting for write response...", [WriteResponse]),
      {noreply, State}
end;
handle_cast({on_message, DeleteResponse}, #state{status = deleting, request_id = RequestId} = State) ->
  case satori_pdu:delete_response(RequestId, DeleteResponse) of
    {ok, Position} -> lager:info("DELETE SUCCESS at position ~p", [Position]),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {error, {Error, Reason}} ->
      lager:warning("DELETE ERROR: ~p, REASON: ~p", [Error, Reason]),
      {noreply, State#state{request_id = RequestId + 1, status = ready}};
    {other, DeleteResponse} -> lager:error("Unexpected delete response ~p. Continue waiting for delete response...", [DeleteResponse]),
      {noreply, State}
  end.

handle_info(_Info, State) ->
  lager:warning("Don't know how to handle_info(~p, ~p)", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


