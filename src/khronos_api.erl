-module(khronos_api).

%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Hello!">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.