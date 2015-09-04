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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Hierarchy:
%%
%%       app
%%        |
%%       sup
%%     /  |  \
%%  api  data  monitor_sup
%%             /     \
%%      monitor_1   monitor_2
init([]) ->
%%   Api = ?CHILD(khronos_api, worker),
  Data = ?CHILD(khronos_data, worker),
  MonitorSup = ?CHILD(khronos_monitor_sup, supervisor),

  {ok, { {one_for_one, 5, 60}, [Data, MonitorSup]} }.

