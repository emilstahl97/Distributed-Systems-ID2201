% module for handling failures
-module(node3).
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
  node(Id, Predecessor, Successor, storage:create(), nil).

% we are the first node
connect(Id, nil) ->
  {ok, {Id, nil, self()}};   % we are our own successor

% connect to existing ring
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
    receive
      {Qref, Skey} ->
        Sref = monitor(Peer),     % monitor the successor
        {ok, {Skey, Sref, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

% send a stabilize message after som time, so new nodes are quickly linked into the ring
% called on when a node is created
schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).   % sends a stabilize message to self() repeatedly after ?Stabilize milliseconds

% function for a node
% a node will have a key, a predecessor and a successor
node(Id, Predecessor, Successor, Store, Next) ->
    receive
      % peer wants to know our key (Id)
      {key, Qref, Peer} ->
          Peer ! {Qref, Id},
          node(Id, Predecessor, Successor, Store, Next);
      % a new node informs us of its existence
      % could be a possible new predecessor
      {notify, New} ->
          {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
          node(Id, Pred, Successor, UpdatedStore, Next);
      % predecessor needs to know our predecessor
      {request, Peer} ->
          request(Peer, Predecessor, Successor),
          node(Id, Predecessor, Successor, Store, Next);
      % our successor informs us about its predecessor
      {status, Pred, Nx} ->
          {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),  % get the successor of the new node
          node(Id, Predecessor, Succ, Store, Nxt);
      % stabilize message sent from schedule_stabilize
      stabilize ->
          stabilize(Successor),
          node(Id, Predecessor, Successor, Store, Next);

      % create a probe message
      probe ->
        create_probe(Id, Successor),
        node(Id, Predecessor, Successor, Store, Next);
      % if the probe message is equal to the Id of the node
      % we know that we sent the probe and can report the time it took to pass it around the ring
      {probe, Id, Nodes, T} ->
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor, Store, Next);
      % if the probe is not ours we forward it to our successor
      % add our own pid to the list of nodes
      {probe, Ref, Nodes, T} ->
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor, Store, Next);

      % a request to store a new key-value
      {add, Key, Value, Qref, Client} ->
        Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added, Next);

      % request to lookup a key in the storage
      {lookup, Key, Qref, Client} ->
        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store, Next);

      % a message to hand over key-value Elements to a new node
      {handover, Elements} ->
        Merged = storage:merge(Store, Elements),
        node(Id, Predecessor, Successor, Merged, Next);

      % if a node fails
      {'DOWN', Ref, process, _, _} ->
        {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
        node(Id, Pred, Succ, Store, Nxt);

      % state of node
      state ->
        io:format("Id: ~w~n", [Id]),
        io:format("Predecessor: ~p, Successor: ~p, Next: ~p~n", [Predecessor, Successor, Next]),
        node(Id, Predecessor, Successor, Store, Next);

      stop ->
        ok
    end.

% handover some storage
handover(Id, Store, Nkey, Npid) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

% add a new key-value to the Store
% a node should take care of all keys from predecessor pid to the pid of itself
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
      true ->
        Client ! {Qref, ok},
        storage:add(Key, Value, Store);   % store the key-value
      false ->
        Spid ! {add, Key, Value, Qref, Client}, % send add message to successor instead
        Store
    end.

% lookup a key in the store and send the reply to the requester
lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
      true ->
        Result = storage:lookup(Key, Store), % get the key if it exist
        Client ! {Qref, Result};
      false ->
        {_,_, Spid} = Successor,
        Spid ! {lookup, Key, Qref, Client}
    end.

% create a probe message with a time stamp
create_probe(Id, {_, _, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% calculate the time it took to pass a probe message around the ring
remove_probe(Time, Nodes) ->
  T = erlang:system_time(micro_seconds) - Time,
  io:format("Probe time: ~w micro seconds ~n Nodes: ~w", [T, Nodes]).

% forward the probe message to the successor
forward_probe(Ref, Time, Nodes, Id, {_, _, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.

% node Id checks the predecessor Pred of its Successor
% sends a suggestion of the new node Id to be the new Predecessor of Successor
% return the successor of the new node
stabilize(Pred, Id, Successor, Next) ->

  % the successor
  % Sref is added for the monitoring
  {Skey, Sref, Spid} = Successor,

    case Pred of
      % if predecessor is empty, inform the successor about our existence
      % node suggests itself as a predecessor
      nil ->
        Spid ! {notify, {Id, self()}},
        {Successor, Next};
      % if the predecessor is pointing to us, do nothing
      {Id, _} ->
        {Successor, Next};
      % if the predecessor is pointing to itself, notify the successor it about our existence
      {Skey, _} ->
        Spid ! {notify, {Id, self()}},
        {Successor, Next};

      % if predecessor is pointing to another node
      {Xkey, Xpid} ->
        % should we position the node between the nodes or not?
        case key:between(Xkey, Id, Skey) of
            % if a node's predecessor lies between the node and the successor
            % the Pred is the new Successor of our node
            true ->
              Xpid ! {request, self()},
      		    Xref = monitor(Xpid),
      		    drop(Sref),
      		    {{Xkey, Xref, Xpid}, {Skey, Spid}};
            % otherwise we should be in between the nodes
            false ->
              Spid ! {notify, {Id, self()}},  % node suggest itself as predecessor
              {Successor, Next}
        end
    end.

% sends request message to its successor
stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

% inform Peer about the Predecessor
request(Peer, Predecessor, {Skey,Sref,Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey,Spid}}
  end.

% being notified of a node is a way for a new node {Nkey, Npid} to make a proposal that it might be our predecessor
% decide on and return the predecessor
notify({Nkey, Npid}, Id, Predecessor, Store) ->

  case Predecessor of
      % we don't have a predecessor
      nil ->
          Keep = handover(Id, Store, Nkey, Npid),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep};   % new node is predecessor with
      % we already have a predecessor
      {Pkey, Pref, _} ->
        % check if new node should be predecessor
        case key:between(Nkey, Pkey, Id) of
          true ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            drop(Pref),
            {{Nkey, Nref, Npid}, Keep};  % new predecessor from new node, and new storage
          false ->
            {Predecessor, Store}  % otherwise we should not change predecessor
          end
  end.

% monitor a node
monitor(Pid) ->
  erlang:monitor(process, Pid).

% demonitor a node
drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

% predecessor has died
down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

% successor has died
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  % monitor the new successor
io:format("Successor of ~w died~n", [Ref]),
  Nref = monitor(Npid),
  % to repair the ring
  Npid ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}. % use Next as new successor