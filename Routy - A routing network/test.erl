-module(test).
-compile(export_all).

start() ->

    routy:start(f1, helsinki),
    routy:start(f2, rovaniemi),
    routy:start(f3, tampere),
	routy:start(f4, turku),

    f1 ! {add, rovaniemi, {f2, 'sweden@2.248.87.26'}},
    f3 ! {add, rovaniemi, {f2, 'sweden@2.248.87.26'}},
    f2 ! {add, tampere, {f3, 'sweden@2.248.87.26'}},
    f4 ! {add, tampere, {f3, 'sweden@2.248.87.26'}},
    f3 ! {add, turku, {f4, 'sweden@2.248.87.26'}},

    f1 ! broadcast,
    timer:sleep(100),
    f2 ! broadcast,
    timer:sleep(100),
    f3 ! broadcast,
    timer:sleep(100),
	f4 ! broadcast,
    timer:sleep(100),

    f1 ! update,
    timer:sleep(100),
    f2 ! update,
    timer:sleep(100),
    f3 ! update,
	timer:sleep(100),
    f4 ! update.

stop() ->
    routy:stop(f1),
    routy:stop(f2),
    routy:stop(f3),
	routy:stop(f4).