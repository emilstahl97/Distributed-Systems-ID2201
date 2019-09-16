
-module(dijkstra).

-compile(export_all).

%% Returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		{_,N,_} ->
			N;
		false ->
			0
	end. 

%% Replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted)->
	%% Keyfind finds the tubple with Node in position 1 in list Sorted and replaces it with the new tuple {Node, N, Gateway}
	%% Keysort sorts by the second element in the tuple, which is the number of hops
	lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})). 

%% Update list if the new path is shorter.
update(Node, NewLength, Gateway, Sorted)->
	case entry(Node, Sorted) of

		%% If the new length is shorter than the old length, replace the element in the list.
		OldLength when NewLength<OldLength ->
			replace(Node, NewLength, Gateway, Sorted);
		%% Otherwise, just leave the list as it is.
		_->
			Sorted
	end.
	
%% Construct a table given a sorted list of nodes, a map and a table constructed so far.
%% If there are no more entries in the sorted list then we are done and the given routing table is complete.
iterate([], _, Table)->
	Table;

%% If the first entry is a dummy entry with an infinite path to a city we know that the rest of the sorted list is also 
%% of infinite length and the given routing table is complete.
iterate([{_, inf, _}|_], _, Table)->
	Table;

%% take the first entry in the sorted list, find the nodes in the map reachable from this entry and for each of these nodes 
%% update the Sorted list. The entry that you took from the sorted list is added to the routing table.
iterate([{Node, Length, Gateway}|T], Map, Table)->

	%% Find the nodes in the map reachable from Node
	Reachables = map:reachable(Node, Map),
	
	%% Apply the "update" function on all nodes "N" in the "Reachable" list. 
	%% T is the rest of the list. It is the accumulator because we want to save all our updates in the rest of the list. 
	%% We add 1 to the length as we have found a new path to a node via the same Gateway, that is one jump further.
	UpdatedSorted = lists:foldl(fun(N, Sorted)->update(N, Length+1, Gateway, Sorted) end, T, Reachables),
	
	%% Call itarate recursively with the updated sorted list and add the entry from the sorted list to the routing table
	iterate(UpdatedSorted, Map, [{Node, Gateway}|Table]).
	
%% take a list of gateways and a map and produce a routing table with one entry per node in the map.
table(Gateways, Map)->
	%% List the nodes of the map
	AllNodes = map:all_nodes(Map),
	
	%% Go through all nodes and set the length to inf and the gateway to unknown
	IntialList = lists:map(fun(Node) -> {Node, inf, unknown} end, AllNodes),
	
	%% The entries of the gateways should have length zero and gateway set to itself.
	%% Go through all elements in the list and set Gateway to the node itself and length to 0. 
	%% Apply update on all Gateways. 
	%% InitialList is the accumulator. We add all elements from the update function to this list. This gives us a complete sorted list. 
	SortedList = lists:foldl(fun(Node, List) -> update(Node, 0, Node, List) end, IntialList, Gateways),
	
	%%Call iterate to get actual values from the sorted list. 
	iterate(SortedList, Map, []).

%% Search the routing table and return the gateway suitable to route messages to a node. 
%% If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table)->
	case lists:keyfind(Node, 1, Table) of
		{_, Gateway}->
			{ok, Gateway};
		false->
			notfound
	end.







