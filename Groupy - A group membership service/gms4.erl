-module(gms4).
-compile(export_all).

-define(timeout, 1000).
-define(arghh, 500).
-define(riskOfloosing, 100).

% initiate a process that is the first node in a group
start(Id) ->
    random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init_leader(Id, random:uniform(1000), Self) end)}.

init_leader(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

% start node that should join the group
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        % invitation of new view after joining group
        {view, N, [Leader|Slaves], Group} ->
            Leader ! {ack, Id},
            Master ! {view, Group},
            erlang:monitor(process, Leader),    % monitor the leader
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)  % initial state is slave

    % timeout when waiting for an invitation to join group
    % if the leader crashes
    after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.

% leader procedure
leader(Id, Master, N, Slaves, Group) ->
    receive
    % message from master or peer node
    {mcast, Msg} ->
        gms1:leader_bcast(Id, {msg, N, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, N+1, Slaves, Group);
    % request from node to join group
    {join, Wrk, Peer} ->
        % add the new node to the slaves and the group
        Slaves2 = lists:append(Slaves, [Peer]),
        Group2 = lists:append(Group, [Wrk]),
        gms1:leader_bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
        Master ! {view, Group2},
        leader(Id, Master, N+1, Slaves2, Group2);

    stop ->
        ok
    end.

% slave procedure
% accepts messages from master and leader
% has a sequence number and a copy of last message from leader
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
      % request from master to multicast message
      {mcast, Msg} ->
          Leader ! {mcast, Msg},
          slave(Id, Master, Leader, N, Last, Slaves, Group);
      % request from master to allow new node to join the group
      {join, Wrk, Peer} ->
          Leader ! {join, Wrk, Peer},
          slave(Id, Master, Leader, N, Last, Slaves, Group);
      % multicasted message from leader
      {msg, N, Msg} ->
          Leader ! {ack, Id},
          Master ! Msg,
          slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
      % old message, do nothing with it
      {msg, I, _} when I < N ->
          Leader ! {ack, Id},
          slave(Id, Master, Leader, N, Last, Slaves, Group);
      % multicasted view from leader
      {view, N, [Leader|Slaves2], Group2} ->
          Leader ! {ack, Id},
          Master ! {view, Group2},
          slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
      % when the leader dies
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
            slave(Id, Master, Leader, N, Last, Rest, Group)
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
  case random:uniform(?arghh) of
      ?arghh ->
          io:format("leader ~w: crash~n", [Id]),
          exit(no_luck);
      _ ->
          ok
  end.