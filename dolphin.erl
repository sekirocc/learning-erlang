-module (dolphin).
-compile(export_all).

dolphin() ->
  io:format("starting dolphin."),
  dolphin().

receive_msg_from(Pid) ->
  % 根据pid获取到进程，然后让receive语句与进程关联起来
  receive
    { From, dance } ->
      From ! "how about not",
      receive_msg_from(Pid)
    { From, fish } ->
      From ! "thank you !",
      receive_msg_from(Pid)
    { From, _ } ->
      From ! "I dont understand, sleep now"
  after 0
    io:format("all dolphin message received.")
  end.
