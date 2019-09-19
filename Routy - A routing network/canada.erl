-module(canada).
-export([start/0, stop/0]).

start() ->

    io:format("Initiating cities... montreal, toronto, vancouver, calgary ~n"),
    routy:start(c1, montreal),
    routy:start(c2, toronto),
    routy:start(c3, vancouver),
	routy:start(c4, calgary),

    c1 ! {add, toronto, {c2, 'canada@2.248.87.26'}},
    timer:sleep(100),
    c2 ! {add, montreal, {c1, 'canada@2.248.87.26'}},
    timer:sleep(100),
    c3 ! {add, toronto, {c2, 'canada@2.248.87.26'}},
    timer:sleep(100),
    c2 ! {add, vancouver, {c3, 'canada@2.248.87.26'}},
    timer:sleep(100),
    c4 ! {add, vancouver, {c3, 'canada@2.248.87.26'}},
    timer:sleep(100),
    c3 ! {add, calgary, {c4, 'canada@2.248.87.26'}},

    c1 ! broadcast,
    timer:sleep(100),
    c2 ! broadcast,
    timer:sleep(100),
    c3 ! broadcast,
    timer:sleep(100),
	c4 ! broadcast,
    timer:sleep(100),

    c1 ! update,
    timer:sleep(100),
    c2 ! update,
    timer:sleep(100),
    c3 ! update,
	timer:sleep(100),
    c4 ! update.


stop() ->
    routy:stop(c1),
    routy:stop(c2),
    routy:stop(c3),
	routy:stop(c4).