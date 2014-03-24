-module(websocketsvr).
-compile(export_all).

start() ->
  {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
  {reuseaddr,true},
  {active, true}]),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  wait(Socket).

wait(Socket) ->
  receive
    {tcp, Socket, Data} ->
      io:format("received:~p~n",[Data]),
      Msg = prefix() ++
      "WebSocket-Origin: http://localhost:2246rn" ++
      "WebSocket-Location: ws://localhost:1234/rnrn",
      gen_tcp:send(Socket, Msg),
      loop(Socket);
    Any ->
      io:format("Received:~p~n",[Any]),
      wait(Socket)
  end.

prefix() ->
  "HTTP/1.1 101 Web Socket Protocol HandshakernUpgrade:
  WebSocketrnConnection: Upgradern".

loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      Data1 = unframe(Data),
      io:format("received:~p~n",[Data1]),
      gen_tcp:send(Socket, [0] ++ "hello from erlang" ++ [255]),
      loop(Socket);
    Any ->
      io:format("Received:~p~n",[Any]),
      loop(Socket)
  end.

unframe([0|T]) -> unframe1(T).

unframe1([255]) -> [];
unframe1([H|T]) -> [H|unframe1(T)].