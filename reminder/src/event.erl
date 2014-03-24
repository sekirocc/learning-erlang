-module (event).
-compile (export_all).

-record (state, {server, name="", to_go=[]}).

normalize(N) ->
  Limit = 49*24*3600,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(DateTime={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(DateTime) -
         calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo >= 0 -> ToGo;
            ToGo < 0 -> 0
         end,
  normalize(Secs).


loop(S=#state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    if Next =:= [] ->
         Server ! {done, S#state.name};
       Next =/= [] ->
         loop(S#state{to_go=[Next]})
    end
  end.


start(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
  spawn_link(?MODULE, init, [self(), EventName, DateTime]).

init(Server, Name, DateTime) ->
  loop(#state{server=Server,
              name=Name,
              to_go=time_to_go(DateTime)}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      % io:format("canceled~n"),
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.