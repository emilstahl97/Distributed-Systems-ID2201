-module(rudy_par).
-export([main/1, start/1, stop/0]).

% My main function
main([A]) ->
	Port = list_to_integer(atom_to_list(A)),
	start(Port),
	io:format("Rudy started: ~s mode, Port ~w~n", ["Parallel", Port]).

start(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(rudy), "time to die").

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen), % my code
			gen_tcp:close(Listen),
			ok;
		{error, _Error} ->
			error
	end.

handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			spawn_link(fun() -> request(Client) end), % my code
			handler(Listen); % -''-
		{error, _Error} ->
			error
	end.

request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			Request = http:parse_request(Str), % my code
			Response = reply(Request),
			gen_tcp:send(Client, Response);
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
	timer:sleep(40), % simulate file handling, server side scripting etc.
	http:ok(URI). % my code: ... -> retrieve_html(URI)
