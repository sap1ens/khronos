-module(khronos_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

khronos_data_test_() ->
  [
    {"Check can be created", ?setup(fun create_check/1)},
    {"Check can be deleted", ?setup(fun delete_check/1)}
  ].

start() ->
  {ok, Name} = khronos_data:start_link(),
  Name.

stop(_) ->
  khronos_data:stop().

create_check(_) ->
  khronos_data:create_check(1, tcp, 80, 1000),
  {ok, Check} = khronos_data:get_check(1),

  ?_assertMatch(#check{id = 1, type = tcp, port = 80, interval = 1000}, Check).

delete_check(_) ->
  CreateTwoChecks = fun() ->
    khronos_data:create_check(2, tcp, 80, 1000),
    khronos_data:create_check(3, udp, 8080, 5000),
    {ok, Checks} = khronos_data:get_all_checks(),
    Checks
  end,

  DeleteOne = fun() ->
    khronos_data:delete_check(3),
    {ok, UpdatedChecks} = khronos_data:get_all_checks(),
    UpdatedChecks
  end,

  [?_assertEqual(2, length(CreateTwoChecks())), ?_assertEqual(1, length(DeleteOne()))].
