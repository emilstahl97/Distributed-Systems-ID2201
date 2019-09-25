-module(test).
-export([run/3, run_vect/3]).

run(Sleep, Jitter, Timer) ->
	Log = loggy:start([john, paul, ringo, george]),
	A = worker:start(john, Log, 13, Sleep, Jitter),
	B = worker:start(paul, Log, 23, Sleep, Jitter),
	C = worker:start(ringo, Log, 36, Sleep, Jitter),
	D = worker:start(george, Log, 49, Sleep, Jitter),
	worker:peers(A, [B, C, D]),
	worker:peers(B, [A, C, D]),
	worker:peers(C, [A, B, D]),
	worker:peers(D, [A, B, C]),
	timer:sleep(Timer),
	loggy:stop(Log),
	worker:stop(A),
	worker:stop(B),
	worker:stop(C),
	worker:stop(D).


run_vect(Sleep, Jitter, Timer) ->
	Log = loggy_vect:start([john, paul, ringo, george]),
	A = worker_vect:start(john, Log, 13, Sleep, Jitter),
	B = worker_vect:start(paul, Log, 23, Sleep, Jitter),
	C = worker_vect:start(ringo, Log, 36, Sleep, Jitter),
	D = worker_vect:start(george, Log, 49, Sleep, Jitter),
	worker_vect:peers(A, [B, C, D]),
	worker_vect:peers(B, [A, C, D]),
	worker_vect:peers(C, [A, B, D]), 
	worker_vect:peers(D, [A, B, C]),
	timer:sleep(Timer),
	loggy_vect:stop(Log),
	worker_vect:stop(A),
	worker_vect:stop(B),
	worker_vect:stop(C),
	worker_vect:stop(D).
