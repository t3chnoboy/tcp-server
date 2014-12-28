-module(server).
-export([start_server/1, listen/1, accept/1, main/1]).

start_server(Port) ->
  spawn(server, listen, [Port]).

listen(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  {ok, LSocket} = gen_tcp:listen(Port, [{active, true}, binary]),
  spawn(server, accept, [LSocket]),
  timer:sleep(infinity).

accept(LSocket) ->
  {ok, _} = gen_tcp:accept(LSocket),
  spawn(server, accept, [LSocket]),
  handle().

send_message(Socket, Msg) ->
  io:format("Sending message ~p~n", [Msg]),
  gen_tcp:send(Socket, Msg).

send_time(Socket) ->
  send_message(Socket, utils:formatted_time()).

close_connection(Socket) ->
  io:format("Closing connection~n", []),
  gen_tcp:close(Socket).

handle() ->
  receive
    {tcp, Socket, <<"QUIT\n">>} ->
      close_connection(Socket);
    {tcp, Socket, <<"TIME\n">>} ->
      send_time(Socket),
      handle();
    {tcp, Socket, <<"ECHO ", Msg/binary>>} ->
      send_message(Socket, Msg),
      handle();
    {tcp, _, <<Msg/binary>>} ->
      io:format("Received message ~p~n", [Msg]),
      handle()
  after 30000 ->
      io:format("timeout")
  end.

main(_) ->
  start_server(1337),
  timer:sleep(infinity).
