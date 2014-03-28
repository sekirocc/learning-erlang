-module (evserv_new).
-compile ([debug_info, export_all]).

-record (state, {clients, events}).
-record (event, {name="", description="", pid, timeout={{1970,1,1}, {0,0,0}}}).

%%% client APIs

start_link() ->
  Pid = my_server:start_link(?MODULE, #state{clients=orddict:new(),
                                       events=orddict:new()}),
  erlang:register(?MODULE, Pid),
  Pid.

subscribe(Pid, Client) ->
  my_server:call(Pid, {subscribe, Client}).

add_event(Pid, Name, Description, TimeOut) ->
  my_server:call(Pid, {add, Name, Description, TimeOut}).

done_event(Pid, Name) ->
  my_server:call(Pid, {done, Name}).  

cancel(Pid, Name) ->
  my_server:call(Pid, {cancel, Name}).

listen(Delay) ->
  receive
    M={done, _Name, _Description} ->
      [M|listen(0)]
  after Delay*1000 ->
    []
  end.

init(S) -> S.

% terminate() ->
%   ?MODULE ! shutdown.
% update() ->
%   ?MODULE ! code_update.


%%% Server functions

handle_call({ subscribe, Client }, From, S) ->
  Ref = erlang:monitor(process, Client),
  NewClients = orddict:store(Ref, Client, S#state.clients),
  my_server:reply(From, ok),
  S#state{clients=NewClients};

handle_call({ add, Name, Description, TimeOut }, From, S) ->
  case valid_datetime(TimeOut) of
    true ->
      EventPid = event_new:start_link(whereis(?MODULE), Name, TimeOut),
      NewEvents = orddict:store(Name,
                                #event{name=Name,
                                       description=Description,
                                       pid=EventPid,
                                       timeout=TimeOut}, 
                                S#state.events),
      my_server:reply(From, ok),
      S#state{events=NewEvents};
    false ->
      my_server:reply(From, {error, bad_timeout}),
      S
  end;

handle_call({ cancel, Name }, From, S) ->
  Events = case orddict:find(Name, S#state.events) of
    {ok, E} ->
      event_new:cancel(E#event.pid),
      orddict:erase(Name, S#state.events);
    error -> 
      S#state.events
    end,
  my_server:reply(From, ok),
  S#state{events=Events};

handle_call({done, Name}, From, S) ->
  case orddict:find(Name, S#state.events) of
    {ok, E} ->
      send_to_client({done, E#event.name, E#event.description}, S#state.clients),
      Events = orddict:erase(Name, S#state.events),
      my_server:reply(From, ok),
      S#state{events=Events};
    error ->
      S
  end;

% code_update ->
%   ?MODULE:loop(S);
% Unkown ->
%   io:format("unkown message, ~p~n", [Unkown]),
%   loop(S)

handle_call({terminate}, From, _S) ->
  my_server:reply(From, ok),
  exit(shutdown).



%%% Private functions

send_to_client(Msg, ClientDict) ->
  io:format("in send_to_client"),
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

%% validate datetime for event

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause -> 
      false
  end;
valid_datetime(_) ->
  true.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24,
                           M >= 0, M < 60,
                           S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

