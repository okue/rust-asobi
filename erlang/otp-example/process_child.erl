-module(process_child).
-export([search/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%          API functions                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% 前提:
%% スーパバイザの親は, アプリケーションまたはスーパバイザ.
%% スーパバイザとリンクしているのは, スーパバイザの直接の子供のみ.
%% 子供以外のプロセスが, スーパバイザに対してリンクはできない.
%%
%% 制限:
%% '$initial_call'が, supervisorでなくsupervisor_bridgeの場合は,
%% スーパバイザの子供を追えない.
%%

%%
%% SupRefList must be list of supervisor pid or name.
%%
search(SupRefList) when is_list(SupRefList) ->
  Pids = lists:flatmap(fun(SupRef) -> search(supervisor, SupRef) end, SupRefList),
  lists:sort(Pids).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(supervisor, undefined) -> [];

search(supervisor, SupRef) when is_pid(SupRef) or is_atom(SupRef) ->
  Ref = to_pid(SupRef),
  %% 仮定:
  %%  任意のスーパバイザの子供木t_1, t_2について,
  %%  t_1内のプロセスとt_2内のプロセスの間にリンクはない
  Children = lists:flatmap(
    fun
      ({_Name, Pid, supervisor, _}) ->
        % Not include Ref in this list
        search(supervisor, Pid);
      ({_Name, Pid, worker, _}) ->
        % Not include Ref in this list
        search(worker, Pid, [Ref]) -- [Ref]
    end,
    supervisor:which_children(Ref)),
  % Append Ref to the result
  lists:umerge([Ref], Children).

search(worker, undefined, Acc) -> Acc;

search(worker, Ref, Acc) when (is_pid(Ref) or is_port(Ref)) and is_list(Acc) ->
  {links, AllLinks} =
    case is_pid(Ref) of
      true  -> erlang:process_info(Ref, links);
      false -> erlang:port_info(Ref, links)
    end,
  %% Refがリンクしているプロセスから, Accを除く
  Links = lists:sort(AllLinks) -- Acc,
  % io:format("~p --> ~p ~p~n", [Ref, Acc, Links]),
  MergedAcc = lists:umerge([Ref], Acc),
  case Links of
    [] -> MergedAcc;
    _  ->
      lists:foldl(
        fun(Link, Acc2) ->
          search(worker,
                 Link,
                 lists:umerge(MergedAcc, Acc2))
        end, [], Links)
  end.

to_pid(Ref) when is_pid(Ref) -> Ref;
to_pid(Ref) when is_atom(Ref) -> whereis(Ref).
