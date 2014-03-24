-module (kitchen).
-compile (export_all).

start(L) -> spawn(?MODULE, fridge, L).
fridge(L) ->
  receive
    {From, {store, Food}} -> 
      From ! { self(), ok },
      fridge([Food | L]);
    {From, {take, Food}} ->
      case lists:member(Food, L) of
        true -> 
          From ! { self(), ok },
          fridge(lists:delete(Food, L));
        false ->
          From ! { self(), not_found },
          fridge(L)
      end
  end.

store(PID, Food) ->
  PID ! { self(), { store, Food } },
  receive
    {P, MSG} -> MSG
  end.
take(PID, Food) ->
  PID ! { self(), { take, Food } },
  receive
    {P, MSG} -> MSG
  end.