-module(hello_sup).
-behaviour(supervisor).

-export([
  start_link/0
]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, hello_sup}, hello_sup, []).

init(_Args) ->
  {ok,
    {{one_for_one, 60, 60},
      [
        {hello, {hello, start_link, []},
          permanent,
          brutal_kill,
          worker,
          [hello]}
      ]
  }}.
