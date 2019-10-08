% a node
% predecessor and successor are represented as: {Key, Pid}

-module(node1).
-compile(export_all).

-define(Stabilize, 100).
-define(Timeout, 1000).

% the first node in the ring
start(Id) ->
  start(Id, nil).

% connecting to an existing ring
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

% node is created
init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer), % get successor
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

% we are the first node
connect(Id, nil) ->
  {ok, {Id, self()}};   % we are our own successor

% connect to existing ring
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
    receive
      {Qref, Skey} ->
        {ok, {Skey, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

% send a stabilize message after som time, so new nodes are quickly linked into the ring
% called on when a node is created
schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).   % sends a stabilize message to self() repeatedly after ?Stabilize milliseconds

% function for a node
% a node will in this version have a key, a predecessor and a successor
node(Id, Predecessor, Successor) ->
    receive
      % peer wants to know our key (Id)
      {key, Qref, Peer} ->
          Peer ! {Qref, Id},
          node(Id, Predecessor, Successor);
      % a new node informs us of its existence
      % could be a possible new predecessor
      {notify, New} ->
          Pred = notify(New, Id, Predecessor),
          node(Id, Pred, Successor);
      % predecessor needs to know our predecessor
      {request, Peer} ->
          request(Peer, Predecessor),
          node(Id, Predecessor, Successor);
      % our successor informs us about its predecessor
      {status, Pred} ->
          Succ = stabilize(Pred, Id, Successor),  % get the successor of the new node
          node(Id, Predecessor, Succ);
      % stabilize message sent from schedule_stabilize
      stabilize ->
          stabilize(Successor),
          node(Id, Predecessor, Successor);

      % create a probe message
      probe ->
        create_probe(Id, Successor),
        node(Id, Predecessor, Successor);

      % if the probe message is equal to the Id of the node
      % we know that we sent the probe and can report the time it took to pass it around the ring
      {probe, Id, Nodes, T} ->
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor);
      % if the probe is not ours we forward it to our successor
      % add our own pid to the list of nodes
      {probe, Ref, Nodes, T} ->
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor);

      % state of node
      state ->
        io:format("Id: ~w~n", [Id]),
        io:format("Predecessor: ~p, Successor: ~p~n", [Predecessor, Successor]),
        node(Id, Predecessor, Successor);

      stop ->
        ok
    end.

% create a probe message with a time stamp
create_probe(Id, {Skey, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% calculate the time it took to pass a probe message around the ring
remove_probe(Time, Nodes) ->
  T = erlang:system_time(micro_seconds) - Time,
  io:format("Nodes: ~p~n", [Nodes]),
  io:format("Time: ~w micro seconds~n", [T]).

% forwards probe
forward_probe(Ref, Time, Nodes, Id, {_, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.

% node Id checks the predecessor Pred of its Successor
% sends a suggestion of the new node Id to be the new Predecessor of Successor
% return the successor of the new node
stabilize(Pred, Id, Successor) ->

  % the successor
  {Skey, Spid} = Successor,

    case Pred of
      % if predecessor is empty, inform the successor about our existence
      % node suggests itself as a predecessor
      nil ->
        Spid ! {notify, {Id, self()}},
        Successor;
      % if the predecessor is pointing to us, do nothing
      {Id, _} ->
        Successor;
      % if the predecessor is pointing to itself, notify the successor it about our existence
      {Skey, _} ->
        Spid ! {notify, {Id, self()}},
        Successor;

      % if predecessor is pointing to another node
      {Xkey, Xpid} ->
        % should we position the node between the nodes or not?
        case key:between(Xkey, Id, Skey) of
            % if other node is between the new node and the successor
            % the Pred is the new Successor
            true ->
              Xpid ! {request, self()},
              Pred;
            % otherwise we should be in between the nodes
            false ->
              Spid ! {notify, {Id, self()}},  % node suggest itself as predecessor
              Successor
        end
    end.

% sends request message to its successor
stabilize({_, Spid}) ->
  Spid ! {request, self()}.

% inform Peer about the Predecessor
request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

% being notified of a node is a way for a new node to make a proposal that it might be our predecessor
% decide on and return the predecessor
notify({Nkey, Npid}, Id, Predecessor) ->

  case Predecessor of
      % we don't have a predecessor
      nil ->
          {Nkey, Npid};   % new node is predecessor
      % we already have a predecessor
      {Pkey, _} ->
        % check if new node should be predecessor
        case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid}; % new predecessor from new node
        false ->
          Predecessor % otherwise we should not change predecessor
        end
  end.