-module(hello).
-export([hello/0]).
hello() ->
    receive
      X -> io:format("aaa! surprise, a message: ~s~n", [X])
    end.