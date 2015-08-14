-module(khronos_data).

-define(SERVER, ?MODULE).

-export([start_link/0]).

start_link() -> {ok, spawn_link(fun() -> io:format("TDB Data") end)}.

