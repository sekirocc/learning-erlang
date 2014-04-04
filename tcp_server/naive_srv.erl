-module (naive_srv).
-compile (export_all).

start_server() ->
  Port = 8099,
  Pid = spawn_link(fun() ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, binary]),
    spawn(fun() -> acceptor(ListenSocket) end),
    timer:sleep(infinity)
  end),
  {ok, Pid}.

acceptor(ListenSocket) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(AcceptSocket).

handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, << "quit", _/binary >>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket)
  end.