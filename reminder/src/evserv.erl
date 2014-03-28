-module (evserv).
-compile (export_all).

-record (state, {clients, events}).
-record (event, {name="", description="", pid, timeout={{1970,1,1}, {0,0,0}}}).

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> 
      false
  end;
valid_datetime(_) ->
  false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24,
                           M >= 0, M < 60,
                           S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

loop(S=#state{}) ->
  receive
    {Pid, MsgRef, { subscribe, Client }} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, { add, Name, Description, TimeOut }} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name,
                                    #event{name=Name,
                                           description=Description,
                                           pid=EventPid,
                                           timeout=TimeOut}, 
                                    S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;
    {Pid, MsgRef, { cancel, Name }} ->
      Events = case orddict:find(Name, S#state.events) of
        {ok, E} ->
          event:cancel(E#event.pid),
          orddict:erase(Name, S#state.events);
        error -> 
          S#state.events
        end,
      Pid ! { MsgRef, ok },
      loop(S#state{events=Events});
    shutdown ->
      exit(shutdown);
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_client({done, E#event.name, E#event.description}, S#state.clients),
          Events = orddict:erase(Name, S#state.events),
          loop(S#state{events=Events});
        error ->
          loop(S)
      end;
    code_update ->
      ?MODULE:loop(S);
    Unkown ->
      io:format("unkown message, ~p~n", [Unkown]),
      loop(S)
  end.

send_to_client(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.
start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.
terminate() ->
  ?MODULE ! shutdown.
update() ->
  ?MODULE ! code_update.

subscribe(Pid) ->
  Ref = erlang:monitor(process, Pid),
  ?MODULE ! {self(), Ref, { subscribe, Pid }},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, TimeOut) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
  receive
    {Ref, Msg} -> Msg;
    {Ref, {error, Reason}} -> {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  receive
    M={done, _Name, _Description} ->
      [M|listen(0)]
  after Delay*1000 ->
    []
  end.

init() ->
  loop(#state{clients=orddict:new(),
              events=orddict:new()}).


