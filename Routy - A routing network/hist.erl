
-module(hist).

-compile(export_all).

%% Return a new history, where messages from Name will always be seen as old.
new(Name)->
	[{Name, -1}].

%% Check if message number N from the Node is old or new. If it is old then return old but if it new return {new, Updated}
%% where Updated is the updated history.
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
	%% The node already exists in the history
	{_, N2} ->
		%% Check if the message is newer or older than the one we already have. 
	    if N > N2 ->
			%% If it is newer, update
		    {new, lists:keyreplace(Node, 1, History, {Node, N})};
	       true ->
			%% Otherwise it is older and we just return "old". 
		    old
	    end;
	false  ->
		%% The node didn't already exist, so we add the message for the first time in the history. 
	    {new, [{Node, N}|History]}
    end.
