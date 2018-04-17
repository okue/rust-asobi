-module(hello).
-export([
  start/0,
  stop/0
]).
-define(CONTENT, <<"<html><title>Hello Mochiweb!</title><body>Hello! MochiWeb!</body></html>">>).

start() ->
  Port = 9999,
  mochiweb_http:start(
    [
     {port, Port},
     {loop, fun loop/1}
    ]),
  receive
    _ -> ok
  end.

loop(Req) ->
  Req:ok({"text/html", ?CONTENT}).

stop() ->
  mochiweb_http:stop().
