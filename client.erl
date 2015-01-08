-module(client).
-mode(compile).
-include("config.hrl").
-export([main/1, wait_for_command/1]).

main(_) ->
  connect(?HOST, ?PORT).

connect(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, ?SOCK_OPTS),
  wait_for_command(Socket).

wait_for_command(Socket) ->
  Command = io:get_line(">"),
  handle_command(Command, Socket).

handle_command(Command, Socket) ->
  case Command of
    ("DOWNLOAD " ++ Filename) ->
      download(utils:remove_newline(Filename), Socket),
      wait_for_command(Socket);
    ("UPLOAD " ++ Filename) ->
      upload(utils:remove_newline(Filename), Socket),
      wait_for_command(Socket);
    _ ->
      gen_tcp:send(Socket, Command),
      handle(Socket)
  end.

upload(Filename, Socket) ->
  Path = "downloads/" ++ Filename,
  case file:read_file(Path) of
    {ok, Data} ->
      gen_tcp:send(Socket, [<<"UPLOAD ">>, Filename]),
      {ok, <<Offset:32/integer>>} = gen_tcp:recv(Socket, 0, ?TIMEOUT),
      io:format("Offset: ~B~n", [Offset]),
      <<_:Offset/binary, Payload/binary>> = Data,
      gen_tcp:send(Socket, Payload);
    {error, enoent} ->
      io:format("File not found");
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

download(Filename, Socket) ->
  Offset = filelib:file_size("downloads/" ++ Filename),
  gen_tcp:send(Socket, [<<"DOWNLOAD ", Offset:32/integer>>, Filename]),
  io:format("Start ~s~n", [utils:formatted_time()]),                         %debug info
  {ok, File} = file:open(("downloads/" ++ Filename), [append, raw]),
  handle_download(File, Socket),
  io:format("End ~s~n", [utils:formatted_time()]).                           %debug info

handle_download(File, Socket) ->
  case gen_tcp:recv(Socket, 0, 1000) of
    {ok, <<"DONE">>} ->
      io:format("Done!~n");
    {ok, <<"NOT_FOUND">>} ->
      io:format("File not found!~n");
    {ok, <<?MSG_OOB, Data:32/integer>>} ->
      io:format("Received ~p~n", [Data]),
      handle_download(File, Socket);
    {ok, <<0, Data/binary>>} ->
      file:write(File, Data),
      handle_download(File, Socket);
    {error, closed} ->
      io:format("disconnect..~n");
    {error, timeout} ->
      io:format("timeout..~n");
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

handle(Socket) ->
  case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
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
