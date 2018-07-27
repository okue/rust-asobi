-module(world).
-export([
    hello/0
  , add/2
]).
-include("world.hrl").
-define(plus, +).

hello() ->
  ?GREETING.

add(X, Y) ->
  ok = X,
  C = 1,
  Z = C + X ?plus Y,
  Z + 1.
