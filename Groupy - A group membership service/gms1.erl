%%%----------------------------------------------------------------------------
%%% Group layer
%%%----------------------------------------------------------------------------
-module(gms1).
-define(arghh, 10000000000000000000000000).

% API
-export([
    start/1, init/2, leader/4, leader_hello/2, leader_status/3, leader_bcast/3,
    start/2, slave_hello/3, slave/7
]).

%% Leader

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
	leader_hello(Id, Master),
	leader_status(Id, Slaves = [], Group = [Master]),
    leader(Id, Master, Slaves, Group).

leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            leader_bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            leader_status(Id, Slaves2, Group2),
            leader_bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
		stop -> ok
	end.

leader_hello(Id, Master) ->
	io:format("worker ~w ~w: spawned leader ~w~n", [Id, Master, self()]).

leader_status(Id, Slaves, Group) ->
    io:format("leader ~w: group =~w~n\t  slaves=~w~n", [Id, Group, Slaves]).

leader_bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: arghh~n", [Id]),
            exit(no_luck);
		_ -> ok
	end.


%% Slave

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
	slave_hello(Id, Grp, Master),
    Grp ! {join, Master, self()}, % to someone in the group
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
	end.

slave_hello(Id, Grp, Master) ->
	io:format("worker ~w ~w: spawned slave  ~w to join ~w~n",
    	[Id, Master, self(), Grp]).

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
			ok
	end.