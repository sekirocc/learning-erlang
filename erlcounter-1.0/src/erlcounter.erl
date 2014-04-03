-module (erlcounter).
-behaviour (application).

-export ([start/2, stop/1]).

start(normal, _Args) ->
  erlcounter_sup:start_link().

stop(_State) ->
  ok.
