-module(test).

-export([sample/0]).
-export([decode/1, encode/1]).

sample() ->
  X = test:decode(<<"{\"people\": [{\"name\": \"Devin Torres\", \"age\": 27}]}">>),
  io:format("~p~n", [X]),
  Y = test:encode(X),
  io:format("~p~n", [Y]).

decode(Data) when is_binary(Data) ->
  'Elixir.Poison':'decode!'(Data).

encode(Data) ->
  'Elixir.Poison':'encode!'(Data).
