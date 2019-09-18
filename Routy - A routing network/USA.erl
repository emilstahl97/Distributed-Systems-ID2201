-module(usa).
-export([start/0, stop/0]).

start() ->

    io:format("Initiating cities... losangeles, sanfrancisco, newyork, miami ~n"),
    routy:start(u1, losangeles),
    routy:start(u2, sanfrancisco),
    routy:start(u3, newyork),
	routy:start(u4, miami),

    u1 ! {add, sanfrancisco, {u2, 'usa@CAN'}},
    timer:sleep(100),
    u2 ! {add, losangeles, {u1, 'usa@CAN'}},
    timer:sleep(100),
    u3 ! {add, sanfrancisco, {u2, 'usa@CAN'}},
    timer:sleep(100),
    u2 ! {add, newyork, {u3, 'usa@CAN'}},
    timer:sleep(100),
    u4 ! {add, newyork, {u3, 'usa@CAN'}},
    timer:sleep(100),
    u3 ! {add, miami, {u4, 'usa@CAN'}},

    u1 ! broadcast,
    timer:sleep(100),
    u2 ! broadcast,
    timer:sleep(100),
    u3 ! broadcast,
    timer:sleep(100),
	u4 ! broadcast,
    timer:sleep(100),

    u1 ! update,
    timer:sleep(100),
    u2 ! update,
    timer:sleep(100),
    u3 ! update,
	timer:sleep(100),
    u4 ! update.


stop() ->
    routy:stop(u1),
    routy:stop(u2),
    routy:stop(u3),
	routy:stop(u4).