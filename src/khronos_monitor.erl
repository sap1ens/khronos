-module(khronos_monitor).

-define(SERVER, ?MODULE).

-export([start_link/0]).

start_link() -> {ok, spawn_link(fun() -> io:format("TDB Monitor") end)}.

