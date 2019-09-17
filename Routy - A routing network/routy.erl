%% The router should route messages through a network
%% Maintain a view of the network
%% Construct optimal routing tables

-module(routy).
-compile(export_all).

%% start and register router process under a unique name
start(Reg, Name) ->
  %% register(RegName, PidOrPort)
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  %% unregister removes the registered name RegName associated with a process identifier or a port identifier
  unregister(Node).

%% initiate a new router
init(Name) ->
	%% Just returns an empty list
	Intf = interfaces:new(),
	%% Also returns an empty list
	Map = map:new(),
	
	%% Call table with two empty lists, which will also return an empty list
	Table = dijkstra:table(Intf, Map),
	
	%% Create a history for the new router
	Hist = hist:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
	%% Add a node to interfaces
    {add, Node, Pid} ->
		%% monitor sends a monitor request of type "process" to the entity identified by "Pid",
		%% Monitor the existence of the process identified by Pid.
		%% i.e. we want to monitor a process with the PID "Pid". This process i our node. 
        Ref = erlang:monitor(process,Pid),
		
		%% Add the node to our interfaces
        Intf1 = interfaces:add(Node, Ref, Pid, Intf),
		
		%% Recursively call router with the new node added to the interfaces
        router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
		%% Get the reference for the node from the interface
        {ok, Ref} = interfaces:ref(Node, Intf),
		
		%% Turn off the monitoring of the node by using the reference to the node we got from calling monitor
        erlang:demonitor(Ref),
		
		%% Remove the node from interfaces
        Intf1 = interfaces:remove(Node, Intf),
        router(Name, N, Hist, Intf1, Table, Map);
	  
	%% If a node is unreachable, we get this down message
	%% A 'DOWN' message has the following pattern: {'DOWN', MonitorRef, Type, Object, Info}
	{'DOWN', Ref, process, _,_}->
		%% Get the name of the node that is down/unreachable
		{ok, Down} = interfaces:name(Ref, Intf),
        io:format("~w: exit recived from ~w~n", [Name, Down]),
		
		%% Remove the node that is down from our interfaces
        Intf1 = interfaces:remove(Down, Intf),
        router(Name, N, Hist, Intf1, Table, Map);
	
	%%Receive a link state message 
	{links, Node, R, Links} ->
		%%Check if the message is old or new
		case hist:update(Node, R, Hist) of
			%% In case the message is new
			{new, Hist1} ->
				%% Send the message to all interfaces
				interfaces:broadcast({links, Node, R, Links}, Intf),
				
				%% Update the map
				Map1 = map:update(Node, Links, Map),
				router(Name, N, Hist1, Intf, Table, Map1);
			
			%% In case the message is old, just call router again
			old ->
				router(Name, N, Hist, Intf, Table, Map)
		end;
	  
	%% Update the routing table 
	%% We should do it periodically, maybe every time we receive a link-state message or better every time the map changes.
	update ->
		Table1 = dijkstra:table(interfaces:list(Intf), Map),
		router(Name, N, Hist, Intf, Table1, Map);
	
	%% Manually order our router to broadcast a link-state message.
	broadcast ->
		%% Send link-state message and broadcast it. 
		Message = {links, Name, N, interfaces:list(Intf)},
		interfaces:broadcast(Message, Intf),
		router(Name, N+1, Hist, Intf, Table, Map);
	
	%% As the name of the process matches the name we get as an argument in the function, we know that
	%% the message has actually arrived at the final destination.
	{route, Name, From, Message} ->
		io:format("~w: Received message ~w ~n", [Name, Message]),
		router(Name, N, Hist, Intf, Table, Map);

	%% If the message is not ours we should forward it. If we find a suitable gateway in the routing table we simply 
	%% forward the message to the gateway. If we do not find a routing entry or do not find a interface of a gateway we have
	%% a problem, simply drop the packet and keep smiling.
	{route, To, From, Message} ->
		io:format("~w: routing message of (~w)", [Name, Message]),
		%% Find the gateway we want to route the message to
		case dijkstra:route(To, Table) of
			{ok, Gw} ->
				%% Get the Pid of the gateway
				case interfaces:lookup(Gw, Intf) of
					{ok, Pid} ->
						%% Send the message to the gateway
						Pid ! {route, To, From, Message};
					notfound ->
						ok
				end;
			notfound ->
				ok
		end,
		router(Name, N, Hist, Intf, Table, Map);
	
	%% Initiate the routing of a message without knowing the name of the local router.
	%% We send the message to ourselves, which is just the router that then routes the message to the right process. 
	{send, To, Message} ->
		self() ! {route, To, Name, Message},
		router(Name, N, Hist, Intf, Table, Map);

	%% Receive a status message from the process "From"
    {status, From} ->
		%% Send the status of that process/node to the process that requested the status. 
        From ! {status, {Name, N, Hist, Intf, Table, Map}},
        router(Name, N, Hist, Intf, Table, Map);
    stop ->
        ok
  end.

%% Send a message requesting the status of Pid
requestStatus(Pid)->
	Pid ! {status, self()},
	receive 
		{status, Status}->
			io:format("Status: ~w~n", [Status])
	end. 
