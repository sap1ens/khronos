-module(khronos_api).

-define(SERVER, ?MODULE).

-export([start_link/0]).

start_link() -> {ok, spawn_link(fun() -> io:format("TDB API") end)}.

