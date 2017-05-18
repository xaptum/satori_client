%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 12. May 2017 1:31 PM
%%%-------------------------------------------------------------------
-module(satori_client).
-author("iguberman").
-behaviour(websocket_client_handler).

-record(state, {handler, role, endpoint, role_secret, status, channel, sub_id, position}).

-define(APP_KEY_ENV, "SATORI_APP_KEY").
-define(ENDPOINT_ENV, "SATORI_ENDPOINT").
-define(ROLE_ENV, "SATORI_ROLE").
-define(ROLE_SECRET_ENV, "SATORI_ROLE_SECRET").

-export([
  send_request/2,
  start_link/1,
  start_link/5,
  init/2,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

%%====================================================================
%% API
%%====================================================================

send_request(WebsocketPid, Request) when is_pid(WebsocketPid) ->
  lager:info("websocket_client:cast(~p, ~p)", [WebsocketPid, {text, Request}]),
  websocket_client:cast(WebsocketPid, {text, Request}).

start_link(HandlerPid) ->
  AppKey = os:getenv(?APP_KEY_ENV),
  Endpoint = os:getenv(?ENDPOINT_ENV),
  Role = os:getenv(?ROLE_ENV),
  RoleSecret = os:getenv(?ROLE_SECRET_ENV),

  true = (AppKey =/= false) and (Endpoint =/= false) and (Role =/= false) and (RoleSecret =/= false),
  start_link(HandlerPid, AppKey, Endpoint, Role, RoleSecret).

start_link(HandlerPid, AppKey, Endpoint, Role, RoleSecret) ->
  URL = Endpoint ++ "?appkey=" ++ AppKey,
  websocket_client:start_link(URL, ?MODULE, [HandlerPid, Role, RoleSecret]).

init([Type, Role, RoleSecret], _ConnState) ->
  State = #state{role = Role, role_secret = RoleSecret},
  {ok, HandshakeRequest} = satori_pdu:handshake_request(Role),
  websocket_client:cast(self(), {text, HandshakeRequest}),
  {ok, State#state{status = handshake, handler = Type}}.

websocket_handle({text, HandshakeResp}, _ConnState, #state{status = handshake, role_secret = RoleSecret} = State) ->
  {ok, Nonce} = satori_pdu:get_nonce_from_handshake_response(1, HandshakeResp),
  {ok, AuthRequest} = satori_pdu:auth_request(2, RoleSecret, Nonce),
  lager:info("HANDSHAKE SUCCESS! SENDING AUTH REQUEST: ~p", [AuthRequest]),
  websocket_client:cast(self(), {text, AuthRequest}),
  {ok, State#state{status = authenticate}};
websocket_handle({text, AuthResp}, _ConnState, #state{status = authenticate, handler = Handler} = State) ->
  ok = satori_pdu:auth_response(2, AuthResp),
  lager:info("AUTHENTICATION SUCCESS!"),
  gen_server:cast(Handler, ready),
  {ok, State#state{status = ready}};
websocket_handle({text, Message}, _ConnState, #state{status = ready, handler = Handler} = State) ->
  gen_server:cast(Handler, {on_message, Message}),
  {ok, State};
websocket_handle(UnexpectedMessage, _ConnState, State)->
  lager:error("UnexpectedMessage ~p at ConnState ~p and State ~p", [UnexpectedMessage, _ConnState, State]),
  {error, unexpected_args, State}.

websocket_info(start, _ConnState, State) ->
  {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
  io:format("Websocket closed in state ~p wih reason ~p~n",
    [State, Reason]),
  ok.
