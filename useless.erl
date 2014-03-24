-module(useless).
-export([add/2, hello/0, greets_and_add_two/1]).
-compile([debug_info, export_all]).

-ifdef(DEBUGMODE).
-define(DEBUG(S), io:format("dbg: " ++ S)).
-else.
-define(DEBUG(S), ok).
-endif.


add(A, B) ->
    A + B.

%% Show greetings
%% Simple really
hello() ->
    io:format("Helo, World\n").

greets_and_add_two(X) ->
    hello(),
    add(2, X).