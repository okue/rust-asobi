- module (main).
- export ([main/1, fibo/1, printfibo/1]).

main(_) ->
  printfibo(10).

printfibo(N) ->
  Res = fibo(N),
  io:fwrite("~w ~w~n", [N, Res]).

fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2).
