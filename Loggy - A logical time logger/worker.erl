-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    rand:uniform(Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, vect:zero());
		stop -> 
			ok
	end.

peers(Wrk, Peers) ->
  	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, CurrentTime)->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
			UpdatedTime = vect:inc(Name,vect:merge(CurrentTime,Time)),
            Log ! {log, Name, UpdatedTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, UpdatedTime);
		stop -> ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after 
		Wait ->
			Selected = select(Peers),
			Time = vect:inc(Name,CurrentTime),
            Message = {hello, rand:uniform(100)},
            Selected ! {msg, Time, Message},
			jitter(Jitter),
			Log ! {log, Name, Time, {sending, Message}},
			loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
