-module(norway).
-export([start/0, stop/0]).

start() ->

    io:format("Initiating cities... oslo, bergen, stavanger, trondheim ~n"),
    routy:start(n1, oslo),
    routy:start(n2, bergen),
    routy:start(n3, stavanger),
	routy:start(n4, trondheim),

    n1 ! {add, bergen, {n2, 'norway@2.248.87.26'}},
    timer:sleep(100),
    n2 ! {add, oslo, {n1, 'norway@2.248.87.26'}},
    timer:sleep(100),
    n3 ! {add, bergen, {n2, 'norway@2.248.87.26'}},
    timer:sleep(100),
    n2 ! {add, stavanger, {n3, 'norway@2.248.87.26'}},
    timer:sleep(100),
    n4 ! {add, stavanger, {n3, 'norway@2.248.87.26'}},
    timer:sleep(100),
    n3 ! {add, trondheim, {n4, 'norway@2.248.87.26'}},

    n1 ! broadcast,
    timer:sleep(100),
    n2 ! broadcast,
    timer:sleep(100),
    n3 ! broadcast,
    timer:sleep(100),
	n4 ! broadcast,
    timer:sleep(100),

    n1 ! update,
    timer:sleep(100),
    n2 ! update,
    timer:sleep(100),
    n3 ! update,
	timer:sleep(100),
    n4 ! update.


stop() ->
    routy:stop(n1),
    routy:stop(n2),
    routy:stop(n3),
	routy:stop(n4).