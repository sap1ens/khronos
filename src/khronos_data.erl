-module(khronos_data).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, create_check/3, delete_check/1, get_check/1, get_all_checks/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_check(Id, Type, Interval) ->
  NewCheck = #check{id = Id, type = Type, interval = Interval},
  gen_server:call(?SERVER, {add, NewCheck}).

delete_check(Id) ->
  gen_server:call(?SERVER, {delete, Id}).

get_check(Id) ->
  gen_server:call(?SERVER, {get, Id}).

get_all_checks() ->
  gen_server:call(?SERVER, {get_all}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, []}.

handle_call({add, Check}, _From, State) ->
  {reply, ok, [Check | State]};

handle_call({delete, Id}, _From, State) ->
  NewState = lists:filter(fun(Check) -> Check#check.id =/= Id end, State),
  {reply, ok, NewState};

handle_call({get, Id}, _From, State) ->
  Check = lists:keyfind(Id, #check.id, State),
  Reply = case Check of
    false -> {not_found};
    Result -> {ok, Result}
  end,
  {reply, Reply, State};

handle_call({get_all}, _From, State) ->
  {reply, {ok, State}, State}.

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

