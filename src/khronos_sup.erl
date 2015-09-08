-module(khronos_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

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
  %% TODO: replace I with unique name
  %% TargetWorker = {I, {khronos_monitor, start_link, []}, permanent, 5000, worker, [khronos_monitor]}.

  %% TODO
  %% {ok, Pid} = gen_tracker:find_or_open(khronos_monitor_sup, TargetWorker).
  {ok}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Hierarchy:
%%
%%       app
%%        |
%%       sup
%%     /  |  \
%%  api  data  monitor_sup (gen_tracker)
%%             /     \
%%      monitor_1   monitor_2
init([]) ->
%%   Api = ?CHILD(khronos_api, worker),
  Data = ?CHILD(khronos_data, worker),
  MonitorSup = {khronos_monitor_sup, {gen_tracker, start_link, [khronos_monitor_sup]}, permanent, infinity, supervisor, []},

  {ok, { {one_for_one, 5, 60}, [Data, MonitorSup]} }.

