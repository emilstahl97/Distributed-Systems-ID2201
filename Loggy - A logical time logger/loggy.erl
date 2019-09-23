-module(loggy).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	io:format("~w~n", [Nodes]),
	loop([], time:clock(Nodes)).

loop(Queue, Clock) ->
	receive
		{log, From, Time, Msg} ->
			%log(From, Time, Msg), loop(Queue, Clock);
			Queue1 = lists:keysort(2, [{From, Time, Msg}|Queue]),
			%Queue1 = [{From, Time, Msg}|Queue],
			Clock1 = time:update(From, Time, Clock),
			C1Time = lists:foldr(fun({_N, T}, Acc) -> [T|Acc] end, [], Clock1),
			io:format("clk: ~w~n", [C1Time]),
			{Safe, Unsafe} = safe_split(Queue1, Clock1), % preserves order
			lists:foreach(fun({F, T, M}) -> log(F, T, M) end, Safe),
			loop(Unsafe, Clock1);
		stop ->
			lists:foreach(fun(E) -> io:format("lft: ~w~n", [E]) end, Queue),
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).


%% helper functions

safe_split(Queue, Clock) ->
	lists:foldr(fun(E, Acc) ->
		safe_split_add(E, Clock, Acc) end, {[], []}, Queue
	).

safe_split_add(Entry, Clock, {Safe, Unsafe}) ->
	{_From, Time, _Msg} = Entry,
	case time:safe(Time, Clock) of
		true ->
				{[Entry|Safe], Unsafe};
		false ->
				{Safe, [Entry|Unsafe]}
	end.