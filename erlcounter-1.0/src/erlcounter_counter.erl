-module (erlcounter_counter).
-behaviour (gen_server).

-export ([start_link/4]).
-export ([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-record (state, {dispatcher, ref, file, re}).

start_link(DispatcherPid, Ref, File, Regex) ->
  gen_server:start_link(?MODULE, [DispatcherPid, Ref, File, Regex], []).

init([DispatcherPid, Ref, File, Regex]) ->
  self() ! start,
  {ok, #state{dispatcher=DispatcherPid,
              ref=Ref,
              file=File,
              re=Regex}}.

handle_info(start, S=#state{re=Re, ref=Ref}) ->
  {ok, Bin} = file:read_file(S#state.file),
  Count = erlcounter_lib:regex_count(Re, Bin),
  erlcounter_dispatch:complete(S#state.dispatcher, Re, Ref, Count),
  {stop, normal, S}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.