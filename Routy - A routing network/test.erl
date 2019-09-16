-module(test).
-compile([start/0, stop/0]).

start() ->

    routy:start(r1, stockholm),
    routy:start(r2, kiruna),
    routy:start(r3, gothenburg),
	routy:start(r4, visby),

    r1 ! {add, kiruna, {r2, 'sweden@2.248.87.26'}},
    r3 ! {add, kiruna, {r2, 'sweden@2.248.87.26'}},
    r2 ! {add, gothenburg, {r3, 'sweden@2.248.87.26'}},
    r4 ! {add, gothenburg, {r3, 'sweden@2.248.87.26'}},
    r3 ! {add, visby, {r4, 'sweden@2.248.87.26'}},

    r1 ! broadcast,
    timer:sleep(100),
    r2 ! broadcast,
    timer:sleep(100),
    r3 ! broadcast,
    timer:sleep(100),
	r4 ! broadcast,
    timer:sleep(100),

    r1 ! update,
    timer:sleep(100),
    r2 ! update,
    timer:sleep(100),
    r3 ! update,
	timer:sleep(100),
    r4 ! update.

stop() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
	routy:stop(r4).