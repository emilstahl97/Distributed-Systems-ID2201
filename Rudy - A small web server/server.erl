-module(server).

-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun () -> rudy:init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").
