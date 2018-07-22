-module(hello_test).
-export([test/0]).

test() ->
  hello_sup:start_link(),
  case whereis(hoge_process) of
    undefined -> erlang:error(no_hoge_process);
    _ -> ok
  end.
