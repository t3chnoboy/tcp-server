-module(server).
-include("config.hrl").
-mode(compile).
-export([start_server/1, listen/1, accept/1, main/1]).

start_server(Port) ->
  spawn(server, listen, [Port]).

listen(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  case gen_tcp:listen(Port, ?SOCK_OPTS) of
    {ok, LSocket} ->
      spawn(server, accept, [LSocket]),
      timer:sleep(infinity);
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  spawn(server, accept, [LSocket]),
  handle(Socket).

send_message(Socket, Msg) ->
  io:format("Sending message ~s~n", [Msg]),
  gen_tcp:send(Socket, Msg).

send_time(Socket) ->
  send_message(Socket, utils:formatted_time()).

close_connection(Socket) ->
  io:format("Closing connection~n", []),
  gen_tcp:close(Socket).

send_file(Filename, Offset, Socket) ->
  io:format("Sending File name: ~s; offset: ~B~n", [Filename, Offset]),
  Path = "uploads/" ++ Filename,
  case file:read_file(Path) of
    {ok, <<_:Offset/binary, Data/binary>>} ->
      io:format("Sending file: ~s~n", [Filename]),
      gen_tcp:send(Socket, Data);
    {error, enoent} ->
      gen_tcp:send(Socket, <<"File not found~n">>);
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

receive_file(Filename, Socket) ->
  Offset = filelib:file_size("uploads/" ++ Filename),
  gen_tcp:send(Socket, <<Offset:32/integer>>),
  io:format("Start ~s~n", [utils:formatted_time()]),                         %debug info
  {ok, File} = file:open(("uploads/" ++ Filename), [append, raw]),
  handle_download(File, Socket),
  io:format("End ~s~n", [utils:formatted_time()]),                           %debug info
  ok.

handle_download(File, Socket) ->
  case gen_tcp:recv(Socket, 0, 1000) of
    {ok, <<Data/binary>>} ->
      file:write(File, Data),
      handle_download(File, Socket);
    {error, closed} ->
      io:format("disconnect..~n");
    _ -> ok
  end.

handle(Socket) ->
  case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
    {ok, <<"QUIT\n">>} ->
      close_connection(Socket);
    {ok, <<"TIME\n">>} ->
      send_time(Socket),
      handle(Socket);
    {ok, <<"ECHO ", Msg/binary>>} ->
      send_message(Socket, Msg),
      handle(Socket);
    {ok, <<"DOWNLOAD ", Offset:32/integer, Filename/binary>>} ->
      send_file(binary_to_list(Filename), Offset, Socket),
      handle(Socket);
    {ok, <<"UPLOAD ", Filename/binary>>} ->
      receive_file(binary_to_list(Filename), Socket),
      handle(Socket);
    {ok, _} ->
      send_message(Socket, "Unknown command"),
      handle(Socket);
    {error, closed} ->
      io:format("disconnect..~n");
    {error, timeout} ->
      io:format("timeout..~n");
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

main(_) ->
  start_server(?PORT),
  timer:sleep(infinity).
