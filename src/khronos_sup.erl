-module(khronos_sup).
-behaviour(supervisor).

-include("khronos_data.hrl").

%% API
-export([start_link/0, schedule_check/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

schedule_check(Target) ->
  TargetWorker = {Target#target.id, {khronos_monitor, start_link, [Target]}, permanent, 5000, worker, [khronos_monitor]},

  {ok, Pid} = gen_tracker:find_or_open(khronos_monitor_sup, TargetWorker),
  {scheduled, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Hierarchy:
%%
%%               app
%%                |
%%               sup
%%          /     |     \
%%  api_server  data  monitor_sup (gen_tracker)
%%      |            /     \
%%     api       monitor_1   monitor_2
%%
init([]) ->
  Api = ?CHILD(khronos_api_server, worker),
  Data = ?CHILD(khronos_data, worker),
  MonitorSup = {khronos_monitor_sup, {gen_tracker, start_link, [khronos_monitor_sup]}, permanent, infinity, supervisor, []},

  {ok, { {one_for_one, 5, 60}, [Data, MonitorSup, Api]} }.

