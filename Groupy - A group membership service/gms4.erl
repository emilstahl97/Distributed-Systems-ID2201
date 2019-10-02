-module(gms4).
-define(timeout, 1000).
-define(arghh, 500).
-define(riskOfloosing, 100).
-compile(export_all).


% initiate a process that is the first node in a group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Self) end)}.
    
init(Id, Rnd, Master) ->
        random:seed(Rnd, Rnd, Rnd),
        gms1:leader_hello(Id, Master),
        gms1:leader_status(Id, Slaves = [], Group = [Master]),
        leader(Id, Master, 0, Slaves, Group).

% leader procedure
leader(Id, Master, N, Slaves, Group) ->
      %io:format("~w~n", [N]),
      receive
            {mcast, Msg} ->
                bcast(Id, {msg, N, Msg}, Slaves), % first to next leader
                Master ! Msg,
                leader(Id, Master, N+1, Slaves, Group);
            {join, Wrk, Peer} ->
                Slaves2 = lists:append(Slaves, [Peer]),
                Group2 = lists:append(Group, [Wrk]),
                bcast(Id, {view,N,[self()|Slaves2],Group2}, Slaves2),
                Master ! {view, Group2},
                leader(Id, Master, N+1, Slaves2, Group2);
            stop -> ok
        end.

    start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, random:uniform(1000), Grp, Self) end)}.

        % start node that should join the group
init(Id, Rnd, Grp, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    gms1:slave_hello(Id, Grp, Master),
    Grp ! {join, Master, self()},
    receive
        % invitation of new view after joining group
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),    % monitor the leader
            Leader ! {ack, Id},
            Master ! {view, Group},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)  % initial state is slave

    % timeout when waiting for an invitation to join group
    % if the leader crashes
    after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.


% slave procedure
% accepts messages from master and leader
% has a sequence number and a copy of last message from leader
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


% election procedure
election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        % select first node as leader
        [Self|Rest] ->
            bcast(Id, Last, Rest),  % multicast the last message seen
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group);
        % the first node in the list is the leader
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            gms3:slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

% broadcasts a message to all
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> sendMsg(Node, Msg), crash(Id) end, Nodes).

% message for sending message to node
% if we have not received an acknowledgement message from the node we resend the message to that node
sendMsg(Node, Msg) ->
    case random:uniform(?riskOfloosing) of

      % if a message was lost, no ack will be received
      ?riskOfloosing -> io:format("Message ~w to Node ~w was lost \n", [Msg, Node]);

      % send message to node, ack will be received
      _ -> Node ! Msg

    end,

      receive
          {ack, Id} -> 
          io:format("Node ~w received message ~w with Id = ~w\n", [Node, Msg, Id])
        after 100 ->
          sendMsg(Node, Msg),
          io:format("Resending of message ~w to Node ~w\n", [Msg, Node])
      end.

% a random crash
% an arghh value of 100 means that the system will crash in average once in a hundred
crash(Id) ->
  gms1:crash(Id).