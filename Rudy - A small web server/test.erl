%This file performs a test of performance of a given server and sends
% N number of requests and measures the time in micro seconds. 
% The test is started by invoking the method "bench" with server, port and number of requests as arguments. 
% @Emil Stahl
% Date: September 9th 2019

-module(test).

-export([bench/3, run/3]).

bench(Host, Port, N) ->
    Start = erlang:system_time(micro_seconds), %Start measure execution time
    run(Host, Port, N), %call run method 
    Finish = erlang:system_time(micro_seconds), %stop measuring time
    ExTime = Finish - Start, %calculate execution time
    io:format("The requests took ~w μs to finish ~n", [ExTime]).

%method to handle the requests recursively
run(Host, Port, N) -> 
    if N == 0 -> ok;
       true -> request(Host, Port), run(Host, Port, N-1)
    end.

%method that performs the requests 
request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt), %establish a TCP conncetion to server
    gen_tcp:send(Server, http:get("foo")), %send to socket 
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
      {ok, _} -> ok;
      {error, Error} ->
	  io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server). %close the connection
