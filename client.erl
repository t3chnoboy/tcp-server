-module(client).
-mode(compile).
-include("config.hrl").
-export([main/1, wait_for_command/1]).

main(_) ->
  connect("localhost", ?PORT).

connect(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),
  wait_for_command(Socket).

wait_for_command(Socket) ->
  Command = io:get_line(">"),
  handle_command(Command, Socket),
  handle(Socket).

handle_command(Command, Socket) ->
  case Command of
    ("DOWNLOAD " ++ Filename) ->
      StrippedFilename = string:strip(Filename, right, $\n),
      Offset = filelib:file_size("downloads/" ++ StrippedFilename),
      gen_tcp:send(Socket, [<<"DOWNLOAD ", Offset:32/integer>>, StrippedFilename]);
    _ ->
      gen_tcp:send(Socket, Command)
  end.

download(Filename, Socket) ->
  io:format("Saving file: ~s~n", [Filename]),
  io:format("Start ~s~n", [utils:formatted_time()]),
  {ok, File} = file:open(("downloads/" ++ Filename), [append, raw]),
  handle_download(File, Socket),
  io:format("End ~s~n", [utils:formatted_time()]),
  ok.

handle_download(File, Socket) ->
  case gen_tcp:recv(Socket, 0, 500) of
    {ok, <<Data/binary>>} ->
      file:write(File, Data),
      handle_download(File, Socket);
    {error, closed} ->
      io:format("disconnect..~n");
    _ -> ok
  end.

handle(Socket) ->
  case gen_tcp:recv(Socket, 0, 60000) of
    {ok, <<"FILE ", Filename/binary>>} ->
      download(binary_to_list(Filename), Socket),
      wait_for_command(Socket);
    {ok, <<Msg/binary>>} ->
      io:format("Received message ~s~n", [Msg]),
      wait_for_command(Socket);
    {error, closed} ->
      io:format("disconnect..~n");
    {error, timeout} ->
      io:format("timeout..~n");
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.
