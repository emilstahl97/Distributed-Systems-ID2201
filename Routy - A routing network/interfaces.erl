%% An interface is described by the symbolic name (london), a process reference and a process identifier.

-module(interfaces).

-compile(export_all).

new()->
	[].

%% Add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf)->
	[{Name, Ref, Pid}|Intf].

%% Remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf)->
	lists:keydelete(Name, 1, Intf).

%% find the process identifier given a name, return {ok, Pid} if found otherwise notfound
lookup(Name, Intf)->
	case lists:keyfind(Name, 1, Intf) of
		{Name, _, Pid}->
			{ok, Pid};
		false->
			notfound
	end.

%% find the reference given a name and return {ok,Ref} or notfound.
ref(Name, Intf)->
	case lists:keyfind(Name, 1, Intf) of
		{Name, Ref, _}->
			{ok, Ref};
		false->
			notfound
	end.

%% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf)->
	case lists:keyfind(Ref, 2, Intf) of
		{Name, Ref, _}->
			{ok, Name};
		false->
			notfound
	end.

%% Return a list with all names given the interfaces
list([])->
	[];
list([{Name, _, _}|T])->
	[Name|list(T)].

%% broadcast(Message, Intf)
%% Send the message to all interface processes
broadcast(_, [])->
	[];
broadcast(Message, [{_, _, Pid}|T])->
	Pid ! Message, 
	broadcast(Message, T).

