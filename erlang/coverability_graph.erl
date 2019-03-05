-module(coverability_graph).
-export([main/1]).
-export([build/1, verify/1]).

config() ->
    #{ job_networks => [ #{ name => seq, jobs => job_specs()} ] }.

job_specs() ->
    [ #{ name => source, inputs => [[]], outputs => [[{echo, arg, 1}], [{echo, arg, 2}]] }
    % , #{ name => echo, inputs => [[{arg, 1}]], outputs => [[{sink, arg, 1}], [{sink, arg, 2}]] }
    , #{ name => echo, inputs => [[{arg, 3}]], outputs => [[{sink, arg, 1}]] }
    , #{ name => sink, inputs => [[{arg, 1}]], outputs => [[]] }
    ].

% ==================================== types ===================================
-type job() :: atom().
-type arg() :: atom().
-type input() :: list().
-type output() :: list().
-type job_spec() :: map().
-type job_specs() :: [job_spec()].
-type config() :: map().

-type vertex() :: {job(), arg()}.
-type path() :: [vertex()].
-type marking() :: #{ vertex() := integer() }.

% omega is equal to infinity
-type ex_int() :: integer() | omega.
-type maybe(X) :: false | {true, X}.

% ==================================== API =====================================

main(_) ->
    Conf = config(),
    JobSpecs = get_job_specs(seq, Conf),
    G = build(JobSpecs),
    % application:start(digraph_viewer),
    % digraph_viewer:register(G),
    verify(G).

build(JS0) ->
    {InitMarkingWithP, JS} = init_marking_js(JS0),
    G = init_digraph(InitMarkingWithP),
    ok = build_cg([InitMarkingWithP], JS, G),
    G.

verify(G) ->
    Ms = no_executable_markings(G),
    case {Ms, digraph_utils:is_acyclic(G)} of
        {[M], true} ->
            case is_empty_marking(M) of
                true -> ok;
                false -> {error, "This network may not consume all items"}
            end;
        {[], _} -> {error, "This network may never finishes"};
        {_, false} -> {error, "This network may never finishes"};
        _ -> {error, "This network may not consume all items"}
    end.

% ==================================== INTERNAL ================================

-spec is_empty_marking(marking()) -> boolean().
is_empty_marking(Marking) ->
    lists:all(fun(X) -> X == 0 end, maps:values(Marking)).

-spec get_job_specs(atom(), config()) -> job_specs().
get_job_specs(Name, Conf) ->
    JNs = maps:get(job_networks, Conf),
    L = lists:filtermap(fun(JN) ->
        case Name == maps:get(name, JN) of
            true -> {true, maps:get(jobs, JN)};
            _ -> false
        end
    end, JNs),
    case L of
        [] -> {error, {no_job_network, Name}};
        [JobSpecs] -> JobSpecs
    end.

-spec no_executable_markings(digraph:graph()) -> [marking()].
no_executable_markings(G) ->
    Vs = digraph:vertices(G),
    lists:filter(fun(V) -> digraph:out_degree(G, V) == 0 end, Vs).

-spec init_digraph({marking(), _}) -> digraph:graph().
init_digraph({Marking, _}) ->
    G = digraph:new(),
    digraph:add_vertex(G, Marking),
    G.

-spec queues(job_specs()) -> [vertex()].
queues(JN) when is_list(JN) ->
    P = lists:foldl(fun(Job, Acc) ->
        J = maps:get(name, Job),
        Inputs = maps:get(inputs, Job),
        Outputs = maps:get(outputs, Job),
        Q1 = mapconcat(fun(I) -> i2q(J, I) end, Inputs),
        Q2 = mapconcat(fun o2q/1, Outputs),
        S = sets:from_list(Q1 ++ Q2),
        sets:union(Acc, S)
    end, sets:new(), JN),
    sets:to_list(P).

-spec init_marking_js(job_specs()) -> {{marking(), path()}, job_specs()}.
init_marking_js(JS0) ->
    JS = init_js(JS0),
    Qs = queues(JS),
    Src = get(src_name),
    M = lists:foldl(fun(P, Acc) ->
        case P of
            {Src, arg} -> maps:put(P, 1, Acc);
            _ -> maps:put(P, 0, Acc)
        end
    end, #{}, Qs),
    {{M, [M]}, JS}.

-spec init_js(job_specs()) -> job_specs().
init_js(JN) ->
    lists:map(fun(J) ->
        Inputs = maps:get(inputs, J),
        case is_source(Inputs) of
            true ->
                put(src_name, maps:get(name, J)),
                maps:update(inputs, [[{arg, 1}]], J);
            _ -> J
        end
    end, JN).

-spec build_cg([{marking(), path()}], job_specs(), digraph:graph()) -> ok.
build_cg([], _, _) -> ok;
build_cg(Markings, JN, G) ->
    % case io:get_line("Y/n") of
    %     "n\n" -> exit(normal);
    %     "N\n" -> exit(normal);
    %     _ -> ok
    % end,
    % print(Markings),

    NewMarkingsWithPath =
        lists:flatmap(fun(M) -> new_markings(M, JN) end, Markings),
    NewMarkingsWithPath2 =
        lists:filtermap(fun(M) -> update_graph(G, M) end, NewMarkingsWithPath),
    build_cg(NewMarkingsWithPath2, JN, G).

-spec new_markings({marking(), path()}, job_specs()) -> [{marking(), path(), job()}].
new_markings({M, Path}, JN) ->
    lists:flatmap(fun(J) ->
        NewMarkings = execute_job(M, J),
        lists:map(fun(M2) -> {M2, Path, maps:get(name, J)} end, NewMarkings)
    end, JN).

-spec execute_job(marking(), job_spec()) -> [marking()].
execute_job(Marking, Job) ->
    Inputs = maps:get(inputs, Job),
    Outputs = maps:get(outputs, Job),
    JName = maps:get(name, Job),
    IOs = [ {I, O} || I <- Inputs, O <- Outputs ],
    lists:filtermap(fun({I, O}) ->
        execute_job(Marking, JName, I, O)
    end, IOs).

-spec execute_job(marking(), job(), input(), output()) -> maybe(marking()).
execute_job(M0, J, I, O) ->
    M1 = update_by_consume(M0, J, I),
    case is_valid_marking(M1) of
        false -> false;
        _ ->
            M2 = update_by_produce(M1, O),
            {true, M2}
    end.

-spec is_valid_marking(marking()) -> boolean().
is_valid_marking(Marking) ->
    lists:all(fun(N) -> N >= 0 end, maps:values(Marking)).

-spec update_by_consume(marking(), job(), input()) -> marking().
update_by_consume(M, J, I) ->
    lists:foldl(fun({Arg, N}, Acc) ->
        consume(N, {J, Arg}, Acc)
    end, M, I).

-spec update_by_produce(marking(), output()) -> marking().
update_by_produce(M, O) ->
    lists:foldl(fun({NextJ, Arg, N}, Acc) ->
        produce(N, {NextJ, Arg}, Acc)
    end, M, O).

-spec consume(integer(), vertex(), marking()) -> marking().
consume(N, Queue, Marking) ->
    maps:update_with(Queue, fun(X) -> minus(X, N) end, Marking).

-spec produce(integer(), vertex(), marking()) -> marking().
produce(N, Queue, Marking) ->
    maps:update_with(Queue, fun(X) -> plus(X, N) end, Marking).

-spec update_graph(digraph:graph(),
                   {marking(), path(), job()}) -> maybe({marking(), path()}).
update_graph(G, {Marking, Path, J}) ->
    Marking2 = lists:foldl(fun(M, Acc) ->
        update_omega(Acc, M)
    end, Marking, Path),
    case exist_vertex(G, Marking2) of
        true ->
            add_edge(G, Path, Marking2, J),
            false;
        _ ->
            digraph:add_vertex(G, Marking2),
            add_edge(G, Path, Marking2, J),
            {true, {Marking2, [Marking2 | Path]}}
    end.

-spec add_edge(digraph:graph(), path(), marking(), job()) -> term().
add_edge(_, [], _, _) -> ok;
add_edge(G, [M1|_], M2, J) ->
    case exist_edge(G, M1, M2, J) of
        true -> ok;
        _ -> digraph:add_edge(G, M1, M2, J)
    end.

-spec exist_vertex(digraph:graph(), vertex()) -> boolean().
exist_vertex(G, V) ->
    case digraph:vertex(G, V) of
        false -> false;
        _ -> true
    end.

-spec exist_edge(digraph:graph(), marking(), marking(), job()) -> boolean().
exist_edge(G, M1, M2, J) ->
    Edges = digraph:out_edges(G, M1),
    lists:any(fun(E) ->
        case digraph:edge(G, E) of
            {_, M1, M2, J} -> true;
            _ -> false
        end
    end, Edges).

-spec update_omega(marking(), marking()) -> marking().
update_omega(NewMarking, M) ->
    case is_greater(NewMarking, M) of
        false -> NewMarking;
        _ -> set_omega(NewMarking, M)
    end.

-spec set_omega(marking(), marking()) -> marking().
set_omega(NewMarking, M) ->
    maps:fold(fun(K, V, Acc) ->
        case is_greater(maps:get(K, NewMarking), V) of
            true -> maps:update(K, omega, Acc);
            _ -> Acc
        end
    end, NewMarking, M).

is_source([[]]) -> true;
is_source(_) -> false.

-spec plus(ex_int(), ex_int()) -> ex_int().
plus(omega, N) when is_integer(N) -> omega;
plus(N, omega) when is_integer(N) -> omega;
plus(N1, N2) when is_integer(N1), is_integer(N2) -> N1 + N2.

-spec minus(ex_int(), ex_int()) -> ex_int().
minus(omega, N) when is_integer(N) -> omega;
minus(N, omega) when is_integer(N) -> -1;
minus(N1, N2) when is_integer(N1), is_integer(N2) -> N1 - N2.

-spec is_greater(ex_int() | marking(), ex_int() | marking()) -> boolean().
is_greater(omega, omega) -> true;
is_greater(omega, _) -> true;
is_greater(_, omega) -> false;
is_greater(N1, N2) when is_integer(N1), is_integer(N2) -> N1 > N2;
is_greater(M1, M2) when is_map(M1), is_map(M2) ->
    lists:all(fun(K) ->
        maps:get(K, M1) >= maps:get(K, M2)
    end, maps:keys(M1)).

i2q(J, {Arg, N}) when is_atom(J), is_atom(Arg), is_integer(N) -> {J, Arg};
i2q(J, Arg) when is_atom(J), is_atom(Arg) -> {J, Arg}.
o2q({J, Arg, N}) when is_atom(J), is_atom(Arg), is_integer(N) -> {J, Arg};
o2q({J, Arg}) when is_atom(J), is_atom(Arg) -> {J, Arg}.

mapconcat(Fun, L) ->
    L2 = lists:concat(L),
    lists:map(Fun, L2).

% print(X) -> io:format(">~p~n", [X]).
