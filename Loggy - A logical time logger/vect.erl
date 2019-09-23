-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
	[].

inc(Name, Clock) ->
	Time1 = case lists:keyfind(Name, 1, Clock) of
		{Name, Time} ->
			Time+1;
		false ->
			1
	end,
	lists:keystore(Name, 1, Clock, {Name, Time1}).

merge([], Clock) ->
	Clock;
merge([{Name, Ti}|Rest], Clock) ->
	Time = case lists:keyfind(Name, 1, Clock) of
		{Name, Tj} ->
			max(Ti, Tj);
		false ->
			1
	end,
	merge(Rest, lists:keystore(Name, 1, Clock, {Name, Time})).


leq(Clock1, Clock2) ->
	lists:all(fun(N, T1) ->
		case lists:keyfind(N, 1, Clock2) of
			{N, T2} ->
				T1 =< T2;
			false ->
				T1 =< 0
		end
	end, Clock1).

clock(Nodes) ->
	na.

update(Node, Time, Clock) ->
	na.

safe(Time, Clock)  ->
	na.