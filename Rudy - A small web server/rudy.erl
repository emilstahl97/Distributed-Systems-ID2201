-module(rudy).

-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun () -> init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port,
			Opt)       % Opening the listening socket
	of
      {ok, Listen} ->
	  handler(Listen),
	  gen_tcp:close(Listen),          % Closing the socket
	  ok;
      {error, _Error} -> error
    end.

handler(Listen) ->
    case
      gen_tcp:accept(Listen)          % Accepting incoming request
	of
      {ok, Client} -> request(Client), handler(Listen);
      {error, _Error} -> error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client,
			0),         % Read the input to string
    case Recv of
      {ok, Str} ->
	  Request = http:parse_request(Str),
	  Response = reply(Request),
	  gen_tcp:send(Client, Response);
      {error, Error} ->
	  io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).                          % Close the connection

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40), http:ok(URI).
