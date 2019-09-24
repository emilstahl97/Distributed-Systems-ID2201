-module(worker).
-export([start/5, stop/1, peers/2]).
start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

	stop(Worker) ->
			Worker ! stop.
		init(Name, Log, Seed, Sleep, Jitter) ->
			rand:seed(Seed, Seed, Seed),
			receive
				{peers, Peers} ->
					loop(Name, Log, Peers, Sleep, Jitter);
		stop -> ok
		end.

		peers(Wrk, Peers) ->
				Wrk ! {peers, Peers}.


loop(Name, Log, Peers, Sleep, Jitter)->
		Wait = rand:uniform(Sleep),
		receive
			{msg, Time, Msg} ->
				Log ! {log, Name, Time, {received, Msg}},
				loop(Name, Log, Peers, Sleep, Jitter);
	stop -> ok;
			Error ->
				Log ! {log, Name, time, {error, Error}}
		after Wait ->
				Selected = select(Peers),
				Time = na,
				Message = {hello, rand:uniform(100)},
				Selected ! {msg, Time, Message},
				jitter(Jitter),
				Log ! {log, Name, Time, {sending, Message}},
				loop(Name, Log, Peers, Sleep, Jitter)

	end.


	jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).


select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).
