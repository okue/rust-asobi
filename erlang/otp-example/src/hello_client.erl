-module(hello_client).
-export([main/0, task/1]).

% say_hello() ->
%   gen_server:cast(?MODULE, say_hello).
%
% get_count() ->
%   gen_server:call(?MODULE, get_count).

main() ->
  [spawn(?MODULE, task, [Id]) || Id <- lists:seq(1, 10)].

task(Id) ->
  timer:sleep(100),
  Res = hello:get_count(),
  io:format("I am ~p, ~p~n", [Id, Res]),
  task(Id).
