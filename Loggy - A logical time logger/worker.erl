-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() ->
		init(Name, Logger, Seed, Sleep, Jitter) end).

	stop(Worker) ->
		Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, time:zero());
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, TimeW) ->
	Wait = random:uniform(Sleep),
	receive
		{msg, TimeR, Msg} ->
			TimeW1 = time:inc(Name, time:merge(TimeW, TimeR)), % merge and inc time
			Log ! {log, Name, TimeW1, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, TimeW1);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
		after Wait ->
			TimeW1 = time:inc(Name, TimeW), % inc time
			Selected = select(Peers),
			Message = {hello, random:uniform(555)},
			Selected ! {msg, TimeW1, Message},
			jitter(Jitter),
			Log ! {log, Name, TimeW1, {sending, Message}},
			loop(Name, Log, Peers, Sleep, Jitter, TimeW1)
		end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).