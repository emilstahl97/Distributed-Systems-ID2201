-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Old} ->
      lists:keyreplace(Name, 1, Time, {Name, Old + 1});
    false ->
      [{Name, 1}|Time]
  end.

%% merge timestamps
merge([], Time) ->
  Time;
merge([{Name, Ti}|Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti}|merge(Rest, Time)]
  end.

%% Ti <= Tj
leq([], _) ->
  true;
leq([{Name, Ti}|Rest],Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, lists:keydelete(Name, 1, Time));
        true ->
          false
      end;
    false ->
      false
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {From, RecTime} = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
      {From, _} ->
        lists:keyreplace(From, 1, Clock, {From, RecTime});
      false ->
        [{From, RecTime} | Clock]
    end.

safe(Time, Clock) ->
  leq(Time, Clock).