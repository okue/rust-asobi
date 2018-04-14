-module(call_back).
-export([handle_call/1]).

handle_call(Request) ->
  io:format("print ~p~n",[Request]).
