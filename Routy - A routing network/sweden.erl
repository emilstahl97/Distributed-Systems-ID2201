-module(sweden).
-export([start/0, stop/0]).

start() ->

    io:format("Initiating cities... Stockholm, kiruna, gothenburg, visby ~n"),
    routy:start(s1, stockholm),
    routy:start(s2, kiruna),
    routy:start(s3, gothenburg),
	routy:start(s4, visby),

    s1 ! {add, kiruna, {s2, 'sweden@2.248.87.26'}},
    timer:sleep(100),
    s2 ! {add, stockholm, {s1, 'sweden@2.248.87.26'}},
    timer:sleep(100),
    s3 ! {add, kiruna, {s2, 'sweden@2.248.87.26'}},
    timer:sleep(100),
    s2 ! {add, gothenburg, {s3, 'sweden@2.248.87.26'}},
    timer:sleep(100),
    s4 ! {add, gothenburg, {s3, 'sweden@2.248.87.26'}},
    timer:sleep(100),
    s3 ! {add, visby, {s4, 'sweden@2.248.87.26'}},

    s1 ! broadcast,
    timer:sleep(100),
    s2 ! broadcast,
    timer:sleep(100),
    s3 ! broadcast,
    timer:sleep(100),
	s4 ! broadcast,
    timer:sleep(100),

    s1 ! update,
    timer:sleep(100),
    s2 ! update,
    timer:sleep(100),
    s3 ! update,
	timer:sleep(100),
    s4 ! update.


stop() ->
    routy:stop(s1),
    routy:stop(s2),
    routy:stop(s3),
	routy:stop(s4).