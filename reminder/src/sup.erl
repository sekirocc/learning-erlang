-module (sup).
-compile (export_all).

start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).
start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args }).

loop({M, F, A}) ->
  Pid = apply(M, F, A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown);
    {'EXIT', Pid, Reason} ->
      io:format("Pid:~p exit for Reason: ~p~n", [Pid, Reason]),
      loop({M, F, A})
  end.
