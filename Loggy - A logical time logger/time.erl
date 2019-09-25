-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> 0. 

inc(_Name, T) -> T + 1.

merge(Ti, Tj) -> max(Ti,Tj).

leq(Ti,Tj) -> 
	case Ti =< Tj of
		true -> true;
		false -> false
	end.

clock(Nodes) -> 
	lists:foldl(fun(Node, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).
	
update(Node, Time, Clock) ->
	UpdatedClock = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    lists:keysort(2, UpdatedClock).
							  
safe(Time, [{_,X}|_]) ->
    leq(Time, X).
