-module(khronos_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

create_check_test() ->
  % TODO: extract to a setup method
  {ok, _} = khronos_data:start_link(),

  khronos_data:create_check(1, tcp, 80, 1000),
  {ok, Check} = khronos_data:get_check(1),

  ?assertMatch(#check{id = 1, type = tcp, port = 80, interval = 1000}, Check).

delete_check_test() ->
  khronos_data:create_check(2, tcp, 80, 1000),
  khronos_data:create_check(3, udp, 8080, 5000),
  {ok, Checks} = khronos_data:get_all_checks(),

  % TODO: +1 from previous test
  ?assertEqual(3, length(Checks)),

  khronos_data:delete_check(3),

  {ok, UpdatedChecks} = khronos_data:get_all_checks(),
  ?assertEqual(2, length(UpdatedChecks)).
