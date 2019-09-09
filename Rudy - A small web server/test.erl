-module(test).

-export([bench/3]).

bench(Host, Port, N) ->
    Start = erlang:system_time(micro_seconds),
    run(Host, Port, N),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

run(Host, Port, N) ->
    if N == 0 -> ok;
       true -> request(Host, Port), run(Host, Port, N-1)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
      {ok, _} -> ok;
      {error, Error} ->
	  io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
