-module(loggy_vect).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
   	Logger ! stop.

init(Nodes) ->
	loop(vect:clock(Nodes), []).

loop(Clock, HoldBackQueue) ->
	%% Receive log message
	receive
		{log, From, Time, Msg} ->
			%% Update the clock when we receive a message 
			UpdatedClock = vect:update(From, Time, Clock),
			
			%% Add the new message to the hold-back queue
			UpdatedHBQ = [{From, Time, Msg}|HoldBackQueue],
			
			%% Sort the hold-back queue (needs to be sorted for safe function to work)
			SortedHBQ = lists:keysort(2, UpdatedHBQ),
			
			%% Check if it is safe to log messages and log if it is. Returns the messages that are not safe to log.
			NewHBQ = checkSafetyAndLog(SortedHBQ, UpdatedClock, []),
			
			loop(UpdatedClock, NewHBQ);
		stop ->
			io:format("\nSize of Holdback Queue: ~w~n", [length(HoldBackQueue)]),
			ok
	end.

%% Log the message, which in this case just means printing it. 
log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

%% Check if it is safe to log the message, and if it is, do it. Otherwise, keep it in the hold-back queue as it is not yet safe to print. 
checkSafetyAndLog([], _, Acc)->
	Acc;
checkSafetyAndLog([{From, Time, Msg}|T], Clock, Acc)->
	%% Check safety
	case vect:safe(Time, Clock) of
		%% If it is safe
		true ->
			%% Log message
			log(From, Time, Msg),
			%% Check rest of the queue
			checkSafetyAndLog(T, Clock, Acc);
		false->
			%% Check the rest of the list and add the message that is not safe to log to the accumulator
			checkSafetyAndLog(T, Clock, [{From, Time, Msg}|Acc])
	end.
