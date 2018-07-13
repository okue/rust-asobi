-module(toy_http_server).
-export([start/1]).

start(Port) ->
  Pid = spawn(
    fun() ->
      {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {backlog, 64}]),
      spawn(fun() -> accept(ListenSocket) end),
      timer:sleep(infinity)
    end),
  {ok, Pid}.

accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> accept(ListenSocket) end),
  trace("connect ~p~n", [Socket]),
  loop(Socket).

loop(Socket) ->
  gen_tcp:send(Socket, "> "),
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"bye", _/binary>>} ->
      gen_tcp:send(Socket, "bye bye!\n"),
      trace("bye!~n"),
      gen_tcp:close(Socket);
    {tcp, Socket, Message} ->
      gen_tcp:send(Socket, "hey, " ++ Message),
      trace("msg is ~p~n", [Message]),
      loop(Socket);
    {tcp_closed, Socket} ->
      trace("close ~p~n", [Socket]),
      gen_tcp:close(Socket);
    {tcp_error, Socket, Reason} ->
      trace("Handle error ~p on ~p~n", [Reason, Socket])
  end.

trace(Message) ->
  trace(Message, []).
trace(Message, Args) ->
  io:format(Message, Args).
