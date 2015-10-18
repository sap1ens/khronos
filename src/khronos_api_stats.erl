-module(khronos_api_stats).

%% API
-export([init/3, allowed_methods/2, content_types_provided/2, terminate/3]).

%% Custom callbacks
-export([test_text/2]).

init(_Transport, Req, []) ->
  {upgrade, protocol, cowboy_rest, Req, []}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"plain">>, []}, test_text}
  ], Req, State}.

test_text(Req, State) ->
  {<<"YO!">>, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.