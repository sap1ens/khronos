-module(khronos_monitor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

khronos_monitor_test_() ->
  [
    {"It should call TCP ports targets", ?setup(fun tcp_call/1)}
  ].

start() ->
  {ok, _} = khronos_data:start_link(),

  {ok, _} = khronos_data:create_target(1, tcp, 8871, "google.com", 20000),
  {ok, Target} = khronos_data:get_target(1),

  {ok, Name} = khronos_monitor:start_link(Target),
  Name.

stop(_) ->
  khronos_data:stop(),
  khronos_monitor:stop().

tcp_call(_) ->
  %% should be bigger than timeout
  timer:sleep(6000),

  {ok, Target} = khronos_data:get_target(1),

  ?_assertMatch([#metric{result = {failed, timeout}}], Target#target.metrics).