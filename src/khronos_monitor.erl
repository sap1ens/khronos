-module(khronos_monitor).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, check_tcp/1]).

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

check_tcp(TargetId) ->
  lager:info("Checking tcp for target with id ~s", [TargetId]),

  gen_server:cast(?SERVER, {check_tcp, TargetId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({check_tcp, TargetId}, State) ->
  {ok, Target} = khronos_data:get_target(TargetId),

  {ok, IP} = inet:getaddr(Target#target.address, inet),

  Result = case gen_tcp:connect(IP, Target#target.port, [binary, {active, true}], Target#target.timeout) of
    {ok, Socket} ->
      gen_tcp:close(Socket),

      lager:info("Check for target with id ~s succeeded", [TargetId]),

      {ok};
    {error, Msg} ->
      lager:info("Check for target with id ~s failed", [TargetId]),

      {failed, Msg} %% Example: {error, timeout} or {error, econnrefused}
  end,

  khronos_data:add_metric(TargetId, now(), Result),

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

