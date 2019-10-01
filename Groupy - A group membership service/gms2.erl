-module(gms2).
-define(timeout, 100).

% API
-export([start/1, start/2]).

%% Leader

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    gms1:init(Id, Master).

%% Slave

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    gms1:slave_hello(Id, Grp, Master),
    Grp ! {join, Master, self()}, % to someone in the group
    receive
        {view, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader), % monitor leader for crash
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
            
    after ?timeout ->
           Master ! {error, "No reply received from group"}
                
    end.


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

        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);

        stop ->
            ok
    end.
    
election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of

        % I'm the new leader
        [Self|Rest] ->
            gms1:leader_status(Id, Rest, Group),
            gms1:leader_bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            gms1:leader(Id, Master, Rest, Group);

         % monitor the new leader
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)

    end.
