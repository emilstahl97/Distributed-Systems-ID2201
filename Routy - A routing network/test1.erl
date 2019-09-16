% module for running the program

-module(test1).
-compile(export_all).


start() ->

  Com1 = 'sweden@127.0.0.1',

    % start the routers

    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, malmo),
    routy:start(r4, lulea),
    routy:start(r5, boras),
    routy:start(r6, broo),

    timer:sleep(100),

    % connect them to each other

    %add sthlm to malmö
    r1 ! {add, malmo, {r3, Com1}},
    timer:sleep(100),
    r3 ! {add, stockholm, {r1, Com1}},
    timer:sleep(100),

    %add malmö to borås
    r3 ! {add, boras, {r5, Com1}},
    timer:sleep(100),
    r5 ! {add, malmo, {r3, Com1}},
    timer:sleep(100),

    %add luleå to borås
    r5 ! {add, lulea, {r4, Com1}},
    timer:sleep(100),
    r4 ! {add, boras, {r5, Com1}},
    timer:sleep(100),

    %add lund to luleå
    r2 ! {add, lulea, {r4, Com1}},
    timer:sleep(100),
    r4 ! {add, lund, {r2, Com1}},
    timer:sleep(100),

    %add lund to stockholm),
    r2 ! {add, stockholm, {r1, Com1}},
    timer:sleep(100),
    r1 ! {add, lund, {r2, Com1}},
    timer:sleep(100),

    %add lund to broo
    r2 ! {add, broo, {r6, Com1}},
    timer:sleep(100),
    r6 ! {add, lund, {r2, Com1}},
    timer:sleep(100),

    %add broo to stockholm
    r1 ! {add, broo, {r6, Com1}},
    timer:sleep(100),
    r6 ! {add, stockholm, {r1, Com1}},
    timer:sleep(100),

    %broadcast and update

    r1 ! broadcast,
    timer:sleep(100),
    r2 ! broadcast,
    timer:sleep(100),
    r3 ! broadcast,
    timer:sleep(100),
    r4 ! broadcast,
    timer:sleep(100),
    r5 ! broadcast,
    timer:sleep(100),
    r6 ! broadcast,
    timer:sleep(100),

    r1 ! update,
    timer:sleep(100),
    r2 ! update,
    timer:sleep(100),
    r3 ! update,
    timer:sleep(100),
    r4 ! update,
    timer:sleep(100),
    r5 ! update,
    timer:sleep(100),
    r6 ! update.

stop() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
    routy:stop(r4),
    routy:stop(r5),
    routy:stop(r6).