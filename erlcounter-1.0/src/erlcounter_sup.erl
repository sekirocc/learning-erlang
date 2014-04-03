-module (erlcounter_sup).
-behaviour (supervisor).

-export ([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init([]) ->
  MaxRestart = 5,
  MaxTime = 100,
  {ok, {{one_for_one, MaxRestart, MaxTime},
        [{dispatch, 
            {erlcounter_dispatch, start_link, []},
            transient, 
            60000,
            worker, 
            [erlcounter_dispatch]}]}}.

