-module(khronos_monitor).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, call_udp/1]).

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

%% TODO: move to handle_cast
%% TODO: implement timeout
call_udp(TargetId) ->
  {ok, Target} = khronos_data:get_target(TargetId),

  {ok, IP} = inet:getaddr(Target#target.address, inet),
  Result = case gen_udp:open(Target#target.port, [{ip, IP}]) of
    {ok, _} -> {ok};
    {error, Msg} -> {failed, Msg} %% Example: {error, eaddrnotavail}
  end,

  khronos_data:add_metric(TargetId, now(), Result),

  ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

