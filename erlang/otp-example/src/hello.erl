-module(hello).
-behaviour(gen_server).

-record(state, {count}).

%% API Function Exports

-export([
  start_link/0,
  stop/0,
  say_hello/0,
  get_count/0,
  hoge/0
]).

%% gen_server Function Exports

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% API Function Definitions

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

say_hello() ->
  gen_server:cast(?MODULE, say_hello).

get_count() ->
  gen_server:call(?MODULE, get_count).


%% gen_server Function Definitions

init([]) ->
  process_flag(trap_exit, true),
  io:format("hello:init is called~n"),
  try
    Pid = spawn_link(fun hoge/0),
    io:format("spawn_link hoge process[PID=~p]~n", [Pid]),
    register(hoge_process, Pid)
  of
    _ ->
      io:format("register ~p as hoge_process~n", [whereis(hoge_process)])
  catch
    throw:X ->
      io:format("throw:~p~n", [X]);
    exit:X ->
      io:format("exit:~p~n", [X]);
    error:X ->
      io:format("error:~p~n", [X])
  end,
  {ok, #state{count = 0}}.

handle_call(get_count, _From, #state{count=Count}) -> 
  {reply, Count, #state{count = Count + 1}}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(say_hello, State) ->
  io:format("Hello~n"),
  {noreply, #state{count = State#state.count + 1}}.

handle_info(Info, State) ->
  io:format("Get info => ~p~n", [Info]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("terminating with Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal

hoge() ->
  io:format("hoge [PID=~p] is spawned~n", [self()]),
  timer:sleep(10000),
  hoge().

internal_dummy_func() ->
  hello_hoge:hoge(),
  ok.
