-module(client).
-export([main/1, wait_for_command/1]).

main(_) ->
  connect("localhost", 1337).

wait_for_command(Socket) ->
  Command = io:get_line(">"),
  gen_tcp:send(Socket, Command),
  wait_for_command(Socket).

connect(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  spawn(client, wait_for_command, [Socket]),
  handle(),
  ok.

handle() ->
  receive
    {tcp, _, <<Msg/binary>>} ->
      io:format("Received message ~p~n", [Msg]),
      handle();
    {tcp_closed, _} ->
      closed
  end.
