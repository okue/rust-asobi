-module(hello_test).
-export([test/0]).

test() ->
  application:start(hello),
  case whereis(hoge_process) of
    undefined -> erlang:error(no_hoge_process);
    _ -> ok, io:format("OK~n")
  end,
  os:cmd('ls'),
  ok = application:stop(hello).
