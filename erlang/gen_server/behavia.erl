-module(behavia).
-export([call/2, start/1,loop/1]).

start(Module) ->
  spawn(behavia,loop,[Module]).

loop(Module) ->
  receive
    {call, From, Req} ->
      Res = Module:handle_call(Req),
      From ! {self(), Res},
      loop(Module)
  end.

call(Pid, Req) ->
  Pid ! {call, self(), Req},
  receive
    {Pid, Response} ->
      Response
  end.
