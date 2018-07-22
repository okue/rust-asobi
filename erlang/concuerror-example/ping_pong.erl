-module(ping_pong).
-export([pong/0]).

pong() ->
  Self = self(),
  Pid = spawn(
          fun() ->
            ping(Self)
          end),
  timer:sleep(500),
  register(?MODULE, Pid),
  Res = receive
    ping -> return__ok
  end,
  io:format("~p~n", [Res]).

ping(P) ->
  P ! ping.
