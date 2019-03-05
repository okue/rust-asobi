-module(hello_test).
-export([test/0]).

test() ->
  % hello_app:start(0, 0),
  application:start(hello),
  % case whereis(hoge_process) of
  %   undefined -> erlang:error(no_hoge_process);
  %   _ -> ok, io:format("OK~n")
  % end,
  os:cmd('ls'),
  application:stop(hello).
