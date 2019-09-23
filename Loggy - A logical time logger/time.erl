-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

%% return an initial Lamport value (could it be 0)
zero() ->
	0.

%% return the time T incremented by one (you will probably ignore the Name but
%% we will use it later)
inc(_Name, T) ->
	T+1.

%% merge the two Lamport time stamps (i.e. take the maximum value)
merge(Ti, Tj) ->
	max(Ti, Tj).

%% true if Ti is less than or equal to Tj
leq(Ti, Tj) ->
	Ti =< Tj.

% return a clock that can keep track of the nodes
clock(Nodes) ->
	[{N, zero()} || N <- Nodes].

% return a clock that has been updated given that we have received a log
% message from a node at a given time
update(Node, Time, Clock) ->
	lists:keyreplace(Node, 1, Clock, {Node, Time}).

% is it safe to log an event that happened at a given time, true or false
safe(Time, Clock)  ->
	lists:all(fun({_N, T}) -> leq(Time, T) end, Clock).