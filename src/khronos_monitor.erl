-module(khronos_monitor).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("khronos_data.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Target) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Target], []).

stop() ->
  gen_server:call(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Target | _]) ->
  lager:info("Creating new monitor from ~p", [Target]),

  case Target#target.timeout >= Target#target.interval of
    true -> {stop, timeout_bigger_than_interval};
    false ->
      %% schedule a check right away
      gen_server:cast(?SERVER, check_tcp),
      %% and periodically after that
      erlang:send_after(Target#target.interval, ?SERVER, trigger_check),

      {ok, Target}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(check_tcp, Target) ->
  {ok, IP} = inet:getaddr(Target#target.address, inet),

  Result = case gen_tcp:connect(IP, Target#target.port, [binary, {active, true}], Target#target.timeout) of
    {ok, Socket} ->
      gen_tcp:close(Socket),

      lager:info("Check for target with id ~w succeeded", [Target#target.id]),

      {ok};
    {error, Msg} ->
      lager:info("Check for target with id ~w failed", [Target#target.id]),

      {failed, Msg} %% Example: {error, timeout} or {error, econnrefused}
  end,

  khronos_data:add_metric(Target#target.id, now(), Result),

  {noreply, Target}.

handle_info(trigger_check, State) ->
  gen_server:cast(?SERVER, check_tcp),

  erlang:send_after(State#target.interval, ?SERVER, trigger_check),

  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

