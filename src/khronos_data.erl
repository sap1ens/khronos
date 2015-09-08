-module(khronos_data).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, create_target/5, delete_target/1, get_target/1, get_all_targets/0, add_metric/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).

create_target(Id, Type, Port, Address, Interval) ->
  NewTarget = #target{id = Id, type = Type, port = Port, address = Address, interval = Interval},

  lager:info("Creating new target ~p", [NewTarget]),

  gen_server:call(?SERVER, {add, NewTarget}).

delete_target(Id) ->
  lager:info("Deleting target with id ~s", [Id]),

  gen_server:call(?SERVER, {delete, Id}).

get_target(Id) ->
  lager:info("Returning target with id ~s", [Id]),

  gen_server:call(?SERVER, {get, Id}).

get_all_targets() ->
  lager:info("Returning all targets", []),

  gen_server:call(?SERVER, {get_all}).

add_metric(TargetId, Timestamp, Result) ->
  {ok, Target} = get_target(TargetId),

  ExistingMetrics = Target#target.metrics,
  NewMetric = #metric{timestamp = Timestamp, result = Result},

  lager:info("Adding new metric ~p for target with id ~s", [NewMetric, TargetId]),

  NewTarget = Target#target{metrics = [NewMetric | ExistingMetrics]},

  gen_server:call(?SERVER, {update, NewTarget}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, []}.

handle_call({add, Target}, _From, State) ->
  {reply, {ok, Target}, [Target | State]};

handle_call({delete, TargetId}, _From, State) ->
  NewState = without(State, TargetId),
  {reply, {ok, NewState}, NewState};

handle_call({get, TargetId}, _From, State) ->
  Target = lists:keyfind(TargetId, #target.id, State),
  Reply = case Target of
    false -> {not_found};
    Result -> {ok, Result}
  end,
  {reply, Reply, State};

handle_call({update, NewTarget}, _From, State) ->
  %% TODO: find something better
  StateWithoutTarget = without(State, NewTarget#target.id),
  {reply, {ok, NewTarget}, [NewTarget | StateWithoutTarget]};

handle_call({get_all}, _From, State) ->
  {reply, {ok, State}, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

without(State, TargetId) ->
  lists:filter(fun(Target) -> Target#target.id =/= TargetId end, State).