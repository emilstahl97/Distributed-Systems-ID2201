% This file gathers all start, stop and bench methods in a single file.
% The server is started by typing "server:start(port)." in the Erlang shell. 

-module(server).

-export([start/1, start_par/1, stop/0, stop_par/0, bench/0, bench/3]).

% start sequential server
start(Port) ->
    register(rudy, spawn(fun () -> rudy:init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").

% start parallell server
start_par(Port) ->
    register(rudy_par, spawn(fun () -> rudy_par:init(Port) end)).


stop_par() -> exit(whereis(rudy_par), "time to die").

% bench the active server with predetermined arguments,  
bench() ->
    Start = erlang:system_time(micro_seconds), %Start measure execution time
    test:run(localhost, 8080, 100), %call run method 
    Finish = erlang:system_time(micro_seconds), %stop measuring time
    ExTime = Finish - Start, %calculate execution time
    io:format("The requests took ~w μs to finish ~n", [ExTime]).

bench(Host, Port, N) ->
    Start = erlang:system_time(micro_seconds), %Start measure execution time
    test:run(Host, Port, N), %call run method 
    Finish = erlang:system_time(micro_seconds), %stop measuring time
    ExTime = Finish - Start, %calculate execution time
    io:format("The requests took ~w μs to finish ~n", [ExTime]).
    