-module(test).
-compile(export_all).

start() ->

    routy:start(f1, stockholm),
    routy:start(f2, kiruna),
    routy:start(f3, gothenburg),
	routy:start(f4, visby),

    f1 ! {add, kiruna, {f2, 'sweden@2.248.87.26'}},
    f3 ! {add, kiruna, {f2, 'sweden@2.248.87.26'}},
    f2 ! {add, gothenburg, {f3, 'sweden@2.248.87.26'}},
    f4 ! {add, gothenburg, {f3, 'sweden@2.248.87.26'}},
    f3 ! {add, visby, {f4, 'sweden@2.248.87.26'}},

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