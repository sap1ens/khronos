-module(khronos_monitor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

khronos_monitor_test_() ->
  [
    {"It should call UDP ports targets", ?setup(fun udp_call/1)}
  ].

start() ->
  {ok, _} = khronos_data:start_link(),
  {ok, Name} = khronos_monitor:start_link(),
  Name.

stop(_) ->
  khronos_data:stop(),
  khronos_monitor:stop().

udp_call(_) ->
  {ok, _} = khronos_data:create_target(1, udp, 8888, "google.com", 1000),

  khronos_monitor:call_udp(1),

  {ok, Target} = khronos_data:get_target(1),

  ?_assertMatch([#metric{result = {failed, eaddrnotavail}}], Target#target.metrics).