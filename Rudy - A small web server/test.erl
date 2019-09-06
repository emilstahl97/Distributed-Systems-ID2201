% benchmark program that measures the time it takes to receive answers
% usage: test:bench(localhost, 8080).

-module(test).

-export([bench/2]).

% measure time in micro seconds
bench(Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    run(100, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

% runs request/2 N times
run(N, Host, Port) ->
    if N == 0 -> ok;
       true -> request(Host, Port), run(N - 1, Host, Port)
    end.

% gen_tcp:connect connect to a server on TCP port Port with IP address Host
% gen_tcp:send send back a GET request to server
% gen_tcp:recv read input from server and return it as a string
rudy : request ( Host , Port ) -> Opt = [ list , { active , false } , { reuseaddr , true } ] , { ok , Server } = gen_tcp : connect ( Host , Port , Opt ) , gen_tcp : send ( Server , http : get ( "foo" ) ) , Recv = gen_tcp : recv ( Server , 0 ) , case Recv of { ok , _ } -> ok ; { error , Error } -> io : format ( "test: error: ~w~n" , [ Error ] ) end , gen_tcp : close ( Server ) .

