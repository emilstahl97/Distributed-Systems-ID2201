-module(worker).

-export([start/4, start/5]).

-define(change, 20).
-define(color, {0,0,0}).

% Start a worker given:
%  Id - a unique interger, only used for debugging
%  Module - the module we want to use, i.e. gms1
%  Rnd - a value to seed the random generator
%  Sleep  - for how long should we sleep between proposing state changes

start(Id, Module, Rnd, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Sleep) end).

init(Id,  Module, Rnd, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id]),
    Color = ?color,
    init_cont(Id, Rnd, Cast, Color, Sleep).

% Same as above, but now we join an existing worker
%  Peer - the process id of a worker

start(Id, Module, Rnd, Peer, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Peer, Sleep) end).

init(Id, Module, Rnd, Peer, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id, Peer]),
    {ok, Color} = join(Id, Cast),
    init_cont(Id, Rnd, Cast, Color, Sleep).

% Wait for the first view to be delivered

join(Id, Cast) ->
    receive 
	{view, _} ->
	    Ref = make_ref(),
	    Cast ! {mcast, {state_request, Ref}},
	    state(Id, Ref);

	% catch error
	{error, Reason} ->
		io:format("worker ~w: join failed: ~p~n", [Id, Reason]),
	    exit(join_failed)

    end.

% and then wait for the, state

state(Id, Ref) ->
    receive
	{state_request, Ref} ->
	    receive
		{state, Ref, Color} ->
		    {ok, Color}
	    end;
	_Ignore ->
	    state(Id, Ref)
    end.

% we're either the first worker or has joined an existing group, but
% know we know everything to continue. 
		
init_cont(Id, Rnd, Cast, Color, Sleep) ->
    random:seed(Rnd, Rnd, Rnd),
    Title = "Worker: " ++ integer_to_list(Id),
    Gui = gui:start(Title, self()),
    Gui ! {color, Color}, 
    worker(Id, Cast, Color, Gui, Sleep),
    Cast ! stop,
    Gui ! stop.

% The worker process, 

worker(Id, Cast, Color, Gui, Sleep) ->
    Wait = wait(Sleep),
    receive

	%% Someone wants us to change the color
	{change, N} ->
	    %% io:format("worker ~w change ~w~n", [Id, N]),
	    Color2 = change_color(N, Color),
	    Gui ! {color, Color2},
	    worker(Id, Cast, Color2, Gui, Sleep);

	%% Someone needs to know the state at this point
	{state_request, Ref} ->
	    Cast ! {mcast, {state, Ref, Color}},
	    worker(Id, Cast, Color, Gui, Sleep);

	%% A reply on a state request but we don't care	
	{state, _, _} ->
	    worker(Id, Cast, Color, Gui, Sleep);	    

	%% Someone wants to join our group
	{join, Peer, Gms} ->
	    Cast ! {join, Peer, Gms},
	    worker(Id, Cast, Color, Gui, Sleep);	    

	%% A view, who cares
	{view, _} ->
	    worker(Id, Cast, Color, Gui, Sleep);	    

	%% So I should stop for a while
	freeze ->
	    frozen(Id, Cast, Color, Gui, Sleep);	   

	%% Change the sleep time
	{sleep, Slp} ->
	    worker(Id, Cast, Color, Gui, Slp);	  

	%% That's all folks
	stop ->
	    ok;

	%% Someone from above wants us to multicast a message.
	{send, Msg} ->
	    Cast !  {mcast, Msg},	    
	    worker(Id, Cast, Color, Gui, Sleep);	    

	Error ->
    	    io:format("strange message: ~w~n", [Error]),
	    worker(Id, Cast, Color, Gui, Sleep)

    after Wait ->
	    %% Ok, let's propose a change of colors
	    %% io:format("worker ~w mcast message~n", [Id]),
	    Cast !  {mcast, {change, random:uniform(?change)}},
	    worker(Id, Cast, Color, Gui, Sleep)	    
    end.


frozen(Id, Cast, Color, Gui, Sleep) ->
    receive 
	go ->
	    worker(Id, Cast, Color, Gui, Sleep);
	stop ->
	    ok;

	%% Someone from above wants us to multicast a message.
	{send, Msg} ->
	    Cast !  {mcast, Msg},	    
	    frozen(Id, Cast, Color, Gui, Sleep)
    end.


wait(Sleep) ->
    if 
	Sleep == 0 -> 
	    0; 
	true -> 
	    random:uniform(Sleep) 
    end.

%% Change of color, we rotate RGB and add N. Since we also make a
%% rotations we will end up in very different state if we receive
%% messages in different order. If we have an initial state of {1,2,3}
%% and receive messages 10 and 20 we would end up in either {3,11,22}
%% or {3,21,12} depending on the order. 

change_color(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.