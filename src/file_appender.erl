-module(file_appender).
-import(string, [concat/2, to_integer/1, right/2]).
-import(lists, [keysearch/3]).
-export([start/1, appender/3, append_line/2, read_conf/1]).
-record(config, {termination_interval = 0}).

start(FilePath) ->
  Conf = read_conf("app.config"),
  case file:open(FilePath, [append]) of
    {ok, Fn} -> spawn(fun() -> appender(FilePath, Fn, Conf) end);
    {error, err_value} -> io:fwrite("could not open file ~p. ~p", [FilePath, err_value])
  end.

read_conf(FileName) ->
  {ok, CurrentDir} = file:get_cwd(),
  Conf_path = CurrentDir ++ "/resources/" ++ FileName,
  case file:consult(Conf_path) of
    {ok, Conf} ->
      {_, {_, Value}} = keysearch(termination_interval_ms, 1, Conf),
      Interval = case to_integer(Value) of
                   {error, no_integer} -> 5000;
                   {ParsedInterval, _} -> ParsedInterval
                 end,
      #config{termination_interval = Interval};
    {error, _} ->
      io:fwrite("could not find configuration file, using default configuration"),
      #config{termination_interval = 5000}
  end.

append_line(Pid, Line) ->
  Pid ! {self(), {append, Line}},
  receive
    {Pid, Msg} -> Msg
  end.

appender(FilePath, FHandle, Conf) ->
  receive
    {From, {append, Line}} ->
      From ! {self(), ok},

      case string:right(Line, 1) of
        "\n" -> file:write(FHandle, Line);
        _ -> file:write(FHandle, concat(Line, "\n"))
      end;
    terminate ->
      ok
  after Conf#config.termination_interval ->
    Interval_in_seconds = Conf#config.termination_interval div 1000,
    io:fwrite("file appender for file: ~p\n has not received messages for ~p seconds. shutting down.\n", [FilePath, Interval_in_seconds]),
    timeout
  end.