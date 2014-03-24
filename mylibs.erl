-module(mylibs).
-compile(export_all).

%% hanoi
hanoi(1, Start, End, _) ->
    io:format( "move #1 from ~p to ~p ~n" , [Start, End] );
hanoi(N, Start, End, Extra) ->
    hanoi(N-1, Start, Extra, End),
    io:format( "move #~p from ~p to ~p ~n" , [N, Start, End] ),
    hanoi(N-1, Extra, End, Start).

binary(1) -> "1";
binary(0) -> "0";
binary(N) ->
    K = N div 2,
    B = N rem 2,
    binary(K) ++ binary(B).
 