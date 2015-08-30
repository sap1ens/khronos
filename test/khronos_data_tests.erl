-module(khronos_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("khronos_data.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

khronos_data_test_() ->
  [
    {"It should create targets", ?setup(fun create_target/1)},
    {"It should delete targets", ?setup(fun delete_target/1)},
    {"It should add metrics", ?setup(fun add_metric/1)},
    {"It should add metrics to targets", ?setup(fun add_metric_increment/1)}
  ].

start() ->
  {ok, Name} = khronos_data:start_link(),
  Name.

stop(_) ->
  khronos_data:stop().

create_target(_) ->
  {ok, _} = khronos_data:create_target(1, tcp, 80, {127, 0, 0, 1}, 1000),
  {ok, Target} = khronos_data:get_target(1),

  ?_assertMatch(#target{id = 1, type = tcp, port = 80, interval = 1000}, Target).

delete_target(_) ->
  CreateTwoTargets = fun() ->
    {ok, _} = khronos_data:create_target(2, tcp, 80, {127, 0, 0, 1}, 1000),
    {ok, _} = khronos_data:create_target(3, tcp, 8080, {127, 0, 0, 1}, 5000),
    {ok, Target} = khronos_data:get_all_targets(),
    Target
  end,

  DeleteOne = fun() ->
    {ok, _} = khronos_data:delete_target(3),
    {ok, UpdatedTargets} = khronos_data:get_all_targets(),
    UpdatedTargets
  end,

  [?_assertEqual(2, length(CreateTwoTargets())), ?_assertEqual(1, length(DeleteOne()))].

add_metric(_) ->
  Now = now(),

  AddMetric = fun() ->
    {ok, _} = khronos_data:create_target(1, tcp, 80, {127, 0, 0, 1}, 1000),
    {ok, TargetWithMetric} = khronos_data:add_metric(1, Now, {ok}),
    TargetWithMetric#target.metrics
  end,

  [?_assertEqual([#metric{timestamp = Now, result = {ok}}], AddMetric())].

add_metric_increment(_) ->
  CreateTargetWithEmptyMetrics = fun() ->
    {ok, Target} = khronos_data:create_target(1, tcp, 80, {127, 0, 0, 1}, 1000),
    Target#target.metrics
  end,

  AddMetric = fun() ->
    {ok, TargetWithMetric} = khronos_data:add_metric(1, now(), {ok}),
    TargetWithMetric#target.metrics
  end,

  [?_assertEqual(0, length(CreateTargetWithEmptyMetrics())), ?_assertEqual(1, length(AddMetric()))].
