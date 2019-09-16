%%This module represents a directional map where you should easily be able to update the map and find nodes directly connected to a given node.
-module(map).

-compile(export_all).

new()->
	[].

%% updates the Map to reflect that Node has directional links to all nodes in the list Links. The old entry is removed.
%% Replace an entry in the map with Links to node with a new entry with new links for that node
%% Node is a city and Links is a list of the cities liked to the Node. 
update(Node, Links, Map)->
	
	%% Keydelete delets the tuple where the first element in the tuple matches node, and returns the list without this tuple. 
	MapWithOldNodeDeleted = lists:keydelete(Node, 1, Map),
	
	%% Return the list without the tuple, but with the new tuple added at the head instead. The new tuple still contains the same node, 
	%% as this is the node that is updated, but now has the new links. 
	[{Node, Links}|MapWithOldNodeDeleted].

%% Returns the list of nodes directly reachable from Node.
reachable(Node, Map)->
	%% Finds the tuple where the first element matches Node, and returns this tuple. 
	case lists:keyfind(Node, 1, Map) of
		{Node, Links} ->
			%% We only want to return the Links, not the whole tuple. 
			Links;
		false ->
			[]
	end.

%% Returns a list of all nodes in the map
all_nodes([])->
	[];
all_nodes([{Node, Links}| T])->
	
	%% Extract The first tuple form the map and put the elements (which is a node and a list of links) at the head of a list, 
	%% and call all_nodes recursively with the tail to get all nodes in a single deeplist (list with lists).
	List = [Node, Links|all_nodes(T)],
	
	%% usort (unique sort) sorts the list and removes duplicates. Use flatten to make a single list instead of a deeplist. 
	lists:usort(lists:flatten(List)).




