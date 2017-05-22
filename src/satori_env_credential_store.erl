%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2017 3:51 PM
%%%-------------------------------------------------------------------
-module(satori_env_credential_store).
-author("iguberman").
-behavior(satori_client).

-define(APPKEY_ENV, "SATORI_APPKEY").
-define(ENDPOINT_ENV, "SATORI_ENDPOINT").
-define(ROLE_SECRET_ENV, "SATORI_ROLE_SECRET").

%% API
-export([
  get_appkey/0,
  get_endpoint/0,
  get_role_secret/1
]).

get_appkey()->
  case os:getenv(?APPKEY_ENV) of
    false -> {error, missing_env_appkey};
    AppKey -> {ok, AppKey}
  end.

get_endpoint()->
  case os:getenv(?ENDPOINT_ENV) of
    false -> {error, missing_env_appkey};
    Endpoint -> {ok, Endpoint}
  end.

get_role_secret(Role)->
  EnvRole = replace(Role, $-, $_),
  RoleSecretEnv = EnvRole ++ "_" ++ ?ROLE_SECRET_ENV,
  case os:getenv(RoleSecretEnv) of
    false -> {error, {missing_env_var, RoleSecretEnv}};
    RoleSecret -> {ok, RoleSecret}
  end.

replace(String, CharFrom, CharTo)->
  [ case Char of CharFrom -> CharTo; Other -> Other end || Char <- String].