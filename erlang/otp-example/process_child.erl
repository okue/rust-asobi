-module(process_child).
-export([search/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%          API functions                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% XXX:
%% スーパバイザから下へ辿るときは, supervisor:which_children/1で取得できる
%% プロセスのリストを使う.
%% '$initial_call'が, supervisorでなくsupervisor_bridgeの場合は,
%% スーパバイザの子供を追えない.
%%
%% 普通のプロセスから下へ辿るときは, erlang:process_info/2で取得できる
%% リンクしているプロセスのリストを使う.
%% ポートからのリンクは, erlang:port_info/2で取得する.
%%

%%
%% SupRefList must be list of supervisor pid or name.
%%
search(SupRefList) when is_list(SupRefList) ->
  Pids =
    lists:flatmap(
      fun(SupRef) ->
        SupPid = to_pid(SupRef),
        search(supervisor, SupPid, [SupPid])
      end,
      SupRefList),
  lists:usort(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(_Type, undefined, Acc) -> Acc;

search(supervisor, SupPid, Acc) when is_pid(SupPid) ->
  lists:foldl(
    fun({_Name, Pid, Type, _}, Acc2) ->
      search(Type, Pid, Acc2)
    end,
    lists:umerge([SupPid], Acc),
    supervisor:which_children(SupPid));

search(worker, Pid, Acc) when (is_pid(Pid) or is_port(Pid)) and is_list(Acc) ->
  {links, AllLinks} =
    case is_pid(Pid) of
      true  -> erlang:process_info(Pid, links);
      false -> erlang:port_info(Pid, links)
    end,
  %% Pidがリンクしているプロセスから, Accを除く
  Links = lists:sort(AllLinks) -- Acc,
  io:format("~p --> ~p ~p~n", [Pid, Acc, Links]),
  lists:foldl(
    fun(Link, Acc2) ->
      search(worker, Link, Acc2)
    end,
    lists:umerge([Pid], Acc),
    Links).

to_pid(Ref) when is_pid(Ref) -> Ref;
to_pid(Ref) when is_atom(Ref) -> whereis(Ref).
