-module(gms3).
-define(timeout, 1000).
-export([start/1, start/2, slave/7, init/3]). % API

%% Leader

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    gms1:leader_hello(Id, Master),
    gms1:leader_status(Id, Slaves = [], Group = [Master]),
    leader(Id, Master, 0, Slaves, Group).

% extended with the the argument N
leader(Id, Master, N, Slaves, Group) ->
    %io:format("~w~n", [N]),
    receive
        {mcast, Msg} ->
            gms1:leader_bcast(Id, {msg, N, Msg}, Slaves), % first to next leader
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            gms1:leader_status(Id, Slaves2, Group2),
            gms1:leader_bcast(Id, {view,N,[self()|Slaves2],Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop -> ok
    end.


%% Slave

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    gms1:slave_hello(Id, Grp, Master),
    Grp ! {join, Master, self()}, % Grp = someone in the group
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader), % monitor leader
            Master ! {view, Group},
            Last = {view, N, [Leader|Slaves], Group},
            slave(Id, Master, Leader, N+1, Last, Slaves, Group)

    % timeout on join request
    after ?timeout ->
            Master ! {error, "no reply from group"}

	end.

% extended with two arguments: N and Last
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);

        % updated for reliable multicast, ignore old messages
        {msg, I, Msg} ->
            case I < N of
                true ->
                    slave(Id, Master, Leader, N, Last, Slaves, Group);
                false ->
                    Master ! Msg,
                    Last2 = {msg, I, Msg},
                    slave(Id, Master, Leader, I+1, Last2, Slaves, Group)
            end;
        {view, I, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            Last2 = {view, I, [Leader|Slaves2], Group2},
            slave(Id, Master, Leader, I+1, Last2, Slaves2, Group2);

        % leader crashed
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);

        stop ->
			ok
	end.

% extended with two arguments: N and Last
election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of

        % I'm the new leader
        [Self|Rest] ->
            %io:format("~w~n", [N]),
            gms1:leader_status(Id, Rest, Group),
            gms1:leader_bcast(Id, Last, Rest), % crucial update: broadcast Last
            gms1:leader_bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group);

        % monitor the new leader
        [Leader|Rest] ->
            erlang:monitor(process, Leader), % if already crashed => 'DOWN'
            slave(Id, Master, Leader, N, Last, Rest, Group)

    end.