-module(call_back2).
-export([handle_call/1]).

handle_call(Request) ->
  io:format("PRINT ~p~n",[Request]).
