-module(fizz_buzz_internal).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {count}).
%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------
-export([
  start_link/0,
  stop/0,
  get_list/1
]).
%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

get_list(N) ->
  gen_server:call(?SERVER, {get_list, N}).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->
  {ok, #state{count=0}}.

handle_call({get_list, N}, _From, #state{count=Count}) ->
  L = do(N),
  {reply, L, #state{count=Count+1}}.

handle_cast(stop, State) ->    % this is the first handle_case clause that
    {stop,                     % deals with the stop atom. We instruct the
     normal,                   % gen_server to stop normally and return
     State                     % the current State unchanged.
    };                         % Note: the semicolon here....

handle_cast(say_hello, State) -> % ... becuase the second clause is here to
    io:format("Hello~n"),      % handle the say_hello atom by printing "Hello"
    {noreply,                  % again, this is asynch, so noreply and we also
    #state{count=
      State#state.count+1}
    }.                         % update our state here

handle_info(Info, State) ->      % handle_info deals with out-of-band msgs, ie
    error_logger:info_msg("????~p~n", [Info]), % msgs that weren't sent via cast
    {noreply, State}.          % or call. Here we simply log such messages.

terminate(_Reason, _State) ->  % terminate is invoked by the gen_server
    error_logger:info_msg("terminating~n"), % container on shutdown.
    ok.                        % we log it and acknowledge with ok.

code_change(_OldVsn, State, _Extra) -> % called during release up/down-
    {ok, State}.               % grade to update internal state. 

% internal functions
do(N) ->
  Aux =
    fun
      (X) when X rem 15 == 0 -> fizzbuzz;
      (X) when X rem 3  == 0 -> fizz;
      (X) when X rem 5  == 0 -> buzz;
      (X) -> X
    end,
  lists:map(Aux, lists:seq(1, N)).
