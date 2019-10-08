% local storage
% represented as a list of tuples {Key, Value}

-module(storage).
-compile(export_all).

% create a new store
create() ->
  [].

% add a Key-Value pair to store
add(Key, Value, Store) ->
  [{Key, Value} | Store].

% see if a Key is in the Store
lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

% return a tuple {Updated, Rest}
% Updated has returned true for the between function on list Store
% Rest has returned false for list Store
split(From, To, Store) ->
  lists:partition(fun({Key,Value})-> key:between(Key, From, To) end, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) ->
   Store ++ Entries.