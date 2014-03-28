-module (trade_fsm).
-behaviour (gen_fsm).

%% Public API
-export ([start/1, start_link/1, trade/2, accept_trade/1, 
          make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% Gen_fsm callbacks
-export ([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
          terminate/3, code_change/4,
          % custom state names
          idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
          negotiate/3, wait/2, ready/2, ready/3 ]).

-compile([debug_info, export_all]).

%%% Public API
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%%% FSM TO FSM 
%% ask the other fsm to negotiate
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% forward the client message accepting negotiate
accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% forward the client offer
do_offer(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {do_offer, OwnPid}).

undo_offer(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {undo_offer, OwnPid}).

%% Ask the other side if he's ready to trade
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

am_ready(OwnPid) ->
  gen_fsm:send_event(OwnPid, 'ready!').

not_yet(OwnPid) ->
  gen_fsm:send_event(OwnPid, not_yet).

ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:sync_send_all_state_event(OtherPid, cancel).


-record(state, {name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

init(Name) ->
  {ok, idle, #state{name=Name}}.

notice(#state{name=Name}, Str, Args) ->
  io:format("~s: " ++ Str ++ "~n", [Name|Args]).

unexpected(Msg, State) ->
  io:format("~p receive unkown event ~p while in state ~p~n", [self(), Msg, State]).

idle({ask_negotiate, OtherPid}, S=#state{}) ->
  notice(S, "~p ask for negotiate", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{monitor=Ref, other=OtherPid}};
idle(Event, Data) ->
  unexpected(Event, Data),
  {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{monitor=Ref, other=OtherPid, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, Data),
  {next_state, idle, Data}.


idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiate", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiate", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting_negotiate", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

add(Item, Items) ->
  [Item | Items].

remove(Item, Items) ->
  Items -- [Item].

negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item), 
  notice(S, "cancelling offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% other side do offer a item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
%% other side cancelling offer
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player cancelling ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
%% ask the other side: are you ready
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
  io:format("Other play ready to trade!~n"),
  notice(S,
          "Other player ready to transfer goods:~n"
          "You get ~p, the other player gets ~p",
          [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

%% i am ready to trade, then ask the other side are you read?
negotiate(ready, From, S=#state{other=OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "Asking if ready, waiting.", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side cancelling offer ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

wait(are_you_ready, S=#state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and i am, waiting for same reply", []),
  {next_state, wait, S};
wait(not_yet, S=#state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};

wait('ready!', S=#state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok), % tell our client, the other side is ready
  notice(S, "other side is ready, Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.




priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "committing...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

commit(S=#state{}) ->
  io:format("Transaction completed for ~s. "
            "Items sent are: ~p, Items get are: ~p",
            [S#state.name, S#state.ownitems, S#state.otheritems]).

handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "receive cancel event", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "other side dead", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.
%% Transaction completed.
terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM leaving.", []);
  terminate(_Reason, _StateName, _StateData) ->
  ok.



