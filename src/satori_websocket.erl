%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 12. May 2017 1:31 PM
%%%-------------------------------------------------------------------
-module(satori_websocket).
-author("iguberman").
-behaviour(websocket_client_handler).

-callback get_appkey() -> list().
-callback get_endpoint() -> list().
-callback get_role_secret(Role :: list()) -> list().

-record(state, {message_handler, role, endpoint, role_secret, status, channel, sub_id, position}).

-export([
  send_request/2,
  start_link/2,
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

start_link(SatoriType, Role) ->
  {ok, CredentialStore} = application:get_env(credential_store),
  {ok, AppKey} = CredentialStore:get_appkey(),
  {ok, Endpoint} = CredentialStore:get_endpoint(),
  {ok, RoleSecret} = CredentialStore:get_role_secret(Role),
  true = (AppKey =/= false) and (Endpoint =/= false) and (RoleSecret =/= false),
  URL = Endpoint ++ "?appkey=" ++ AppKey,
  websocket_client:start_link(URL, satori_websocket, [SatoriType, Role, RoleSecret]).

init([SatoriType, Role, RoleSecret], _ConnState) ->
  State = #state{role = Role, role_secret = RoleSecret},
  {ok, HandshakeRequest} = satori_pdu:handshake_request(Role),
  websocket_client:cast(self(), {text, HandshakeRequest}),
  MessageHandler = satori_client:registered_name(SatoriType, Role),
  {ok, State#state{status = handshake, message_handler = MessageHandler}}.

websocket_handle({text, HandshakeResp}, _ConnState, #state{status = handshake, role_secret = RoleSecret} = State) ->
  {ok, Nonce} = satori_pdu:get_nonce_from_handshake_response(1, HandshakeResp),
  {ok, AuthRequest} = satori_pdu:auth_request(2, RoleSecret, Nonce),
  lager:info("HANDSHAKE SUCCESS! SENDING AUTH REQUEST: ~p", [AuthRequest]),
  websocket_client:cast(self(), {text, AuthRequest}),
  {ok, State#state{status = authenticate}};
websocket_handle({text, AuthResp}, _ConnState, #state{status = authenticate, message_handler = Handler} = State) ->
  ok = satori_pdu:auth_response(2, AuthResp),
  lager:info("AUTHENTICATION SUCCESS!"),
  gen_server:cast(Handler, ready),
  {ok, State#state{status = ready}};
websocket_handle({text, Message}, _ConnState, #state{status = ready, message_handler = Handler} = State) ->
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
