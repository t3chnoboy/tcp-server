-module(echo_server).
-export([start_server/1, listen/1, accept/1]).

start_server(Port) ->
  spawn(?MODULE, listen, [Port]).

listen(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, binary]),
  spawn(?MODULE, accept, [ListenSocket]),
  timer:sleep(infinity),
  ok.

accept(ListenSocket) ->
  {ok, _} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, accept, [ListenSocket]),
  handle().

handle() ->
  receive
    {tcp, Socket, <<"quit\r\n">>} ->
      io:format("Closing connection~n", []),
      gen_tcp:close(Socket);
    {tcp, Socket, <<"time\r\n">>} ->
      io:format("Client requesting time~n"),
      gen_tcp:send(Socket, formatted_time()),
      handle();
    {tcp, Socket, <<"ECHO ", Msg/binary>>} ->
      case binary:last(Msg) of
        $\n ->
          io:format("Received message ~p~n", [Msg]),
          gen_tcp:send(Socket, Msg),
          handle();
        _ ->
          handle(Msg)
      end;
    {tcp, _, Msg} ->
      io:format("Received message ~p~n", [Msg]),
      handle()
  end.

handle(Buf) ->
  receive
    {tcp, Socket, <<Msg/binary>>} ->
      io:format("Received part of the message ~p~n", [Msg]),
      case binary:last(Msg) of
        $\n ->
          gen_tcp:send(Socket, [Buf, Msg]),
          handle();
        _ -> handle([Buf, Msg])
      end
  end.


formatted_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  io_lib:format("~p.~p.~p ~p:~p:~p~n", [Day, Month, Year, Hour, Minute, Second]).
