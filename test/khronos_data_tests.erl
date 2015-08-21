-module(khronos_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

khronos_data_test_() ->
  [
    {"It should create checks", ?setup(fun create_check/1)},
    {"It should delete checks", ?setup(fun delete_check/1)},
    {"It should add metrics", ?setup(fun add_metric/1)},
    {"It should add metrics to checks", ?setup(fun add_metric_increment/1)}
  ].

start() ->
  {ok, Name} = khronos_data:start_link(),
  Name.

stop(_) ->
  khronos_data:stop().

create_check(_) ->
  {ok, _} = khronos_data:create_check(1, tcp, 80, 1000),
  {ok, Check} = khronos_data:get_check(1),

  ?_assertMatch(#check{id = 1, type = tcp, port = 80, interval = 1000}, Check).

delete_check(_) ->
  CreateTwoChecks = fun() ->
    {ok, _} = khronos_data:create_check(2, tcp, 80, 1000),
    {ok, _} = khronos_data:create_check(3, udp, 8080, 5000),
    {ok, Checks} = khronos_data:get_all_checks(),
    Checks
  end,

  DeleteOne = fun() ->
    {ok, _} = khronos_data:delete_check(3),
    {ok, UpdatedChecks} = khronos_data:get_all_checks(),
    UpdatedChecks
  end,

  [?_assertEqual(2, length(CreateTwoChecks())), ?_assertEqual(1, length(DeleteOne()))].

add_metric(_) ->
  Now = now(),

  AddMetric = fun() ->
    {ok, _} = khronos_data:create_check(1, tcp, 80, 1000),
    {ok, CheckWithMetric} = khronos_data:add_metric(1, Now, {ok}),
    CheckWithMetric#check.metrics
  end,

  [?_assertEqual([#metric{timestamp = Now, result = {ok}}], AddMetric())].

add_metric_increment(_) ->
  CreateCheckWithEmptyMetrics = fun() ->
    {ok, Check} = khronos_data:create_check(1, tcp, 80, 1000),
    Check#check.metrics
  end,

  AddMetric = fun() ->
    {ok, CheckWithMetric} = khronos_data:add_metric(1, now(), {ok}),
    CheckWithMetric#check.metrics
  end,

  [?_assertEqual(0, length(CreateCheckWithEmptyMetrics())), ?_assertEqual(1, length(AddMetric()))].
