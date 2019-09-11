% This is a simple program that waits for an incoming request, 
% delivers a reply and then terminates.
% @Emil Stahl
% Date: September 9th 2019

-module(rudy).

-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt)       % Opening the listening socket
	of
      {ok, Listen} ->
	  handler(Listen),                % pass socket to handler/1
	  gen_tcp:close(Listen),          % Closing the socket
	  ok;
      {error, _Error} -> error
    end.

% will listen to the socket for an incoming connection
handler(Listen) ->
    case
      gen_tcp:accept(Listen)          % Accepting incoming request
	of
      {ok, Client} -> 
          request(Client), % pass connection to request/1
          handler(Listen);
      {error, _Error} -> error
    end.

% will read the request from the client connection and parse it
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),         % Read all of the input to string
    case Recv of
      {ok, Str} ->
	  Request = http:parse_request(Str), %store the string in Request
	  Response = reply(Request),        %pass the request to reply
	  gen_tcp:send(Client, Response);   %send the response to client socket
      {error, Error} ->
	  io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).    % Close the connection

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40), http:ok(URI).
