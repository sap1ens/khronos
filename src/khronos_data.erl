-module(khronos_data).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, create_check/4, delete_check/1, get_check/1, get_all_checks/0, add_metric/3]).

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

create_check(Id, Type, Port, Interval) ->
  NewCheck = #check{id = Id, type = Type, port = Port, interval = Interval},
  gen_server:call(?SERVER, {add, NewCheck}).

delete_check(Id) ->
  gen_server:call(?SERVER, {delete, Id}).

get_check(Id) ->
  gen_server:call(?SERVER, {get, Id}).

get_all_checks() ->
  gen_server:call(?SERVER, {get_all}).

add_metric(CheckId, Timestamp, Result) ->
  {ok, Check} = get_check(CheckId),

  ExistingMetrics = Check#check.metrics,
  NewMetric = #metric{timestamp = Timestamp, result = Result},
  NewCheck = Check#check{metrics = [NewMetric | ExistingMetrics]},

  gen_server:call(?SERVER, {update, NewCheck}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, []}.

handle_call({add, Check}, _From, State) ->
  {reply, {ok, Check}, [Check | State]};

handle_call({delete, CheckId}, _From, State) ->
  NewState = without(State, CheckId),
  {reply, {ok, NewState}, NewState};

handle_call({get, CheckId}, _From, State) ->
  Check = lists:keyfind(CheckId, #check.id, State),
  Reply = case Check of
    false -> {not_found};
    Result -> {ok, Result}
  end,
  {reply, Reply, State};

handle_call({update, NewCheck}, _From, State) ->
  %% TODO: find something better
  StateWithoutCheck = without(State, NewCheck#check.id),
  {reply, {ok, NewCheck}, [NewCheck | StateWithoutCheck]};

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

without(State, CheckId) ->
  lists:filter(fun(Check) -> Check#check.id =/= CheckId end, State).