- module (sample00).
- export ([main/1, server/0, client/2]).

main(_) ->
    S = spawn(?MODULE, server, [])
  , client(self(), S)
.

server() ->
  receive
    {C, ok} -> C ! good;
    {C, no} -> C ! bad;
    {C, _}  -> C ! invalid;
    ok -> io:format("ERROR~n");
    _  -> io:format("ERROR~n")
  end
  , server()
.

client(Self, S) ->
    S ! {Self, ok}
  , receive
      M -> io:format("~p~n", [M])
    end
  , client(Self, S)
.
