-module(loggy).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
   	Logger ! stop.

init(Nodes) ->
    loop(time:clock(Nodes), []).

loop(Clock, MessageQueue) ->
    receive
        {log, From, Time, Msg} ->
			UpdatedClock = time:update(From,Time,Clock),
			UpdatedQueue = lists:keysort(2, [{From, Time, Msg} | MessageQueue]),
	    	NewQueue = checkQueue(UpdatedQueue, UpdatedClock, []),
            loop(UpdatedClock, NewQueue);
        stop ->
			SortedQueue = lists:keysort(2, MessageQueue),
			lists:foreach(fun({From, Time, Msg}) -> log(From, Time, Msg) end, SortedQueue)
	end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

checkQueue([], _, Queue) ->
    Queue;
checkQueue([{From,Time,Msg}|R], Clock, Queue) ->
    case time:safe(Time, Clock) of
		true ->
	    	log(From, Time, Msg),
	  		checkQueue(R, Clock, Queue);
		false ->
	    	checkQueue(R, Clock, [{From, Time, Msg} | Queue])
    end.
