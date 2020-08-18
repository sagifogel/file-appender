-module(file_appender_with_sleep_tests).
-import(string, [len/1]).
-import(utils, [read_lines/1, get_root_path/0]).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  Path = get_root_path(),
  FilePath = Path ++ "/file3.txt",
  file:delete(FilePath),
  Pid = file_appender:start(FilePath, #{conf => "test.config"}),
  file_appender:append_line(Pid, "1"),
  file_appender:append_line(Pid, "2"),
  file_appender:append_line(Pid, "3"),
  file_appender:append_line(Pid, "4"),
  file_appender:append_line(Pid, "5"),
  timer:sleep(2500),
  file_appender:append_line(Pid, "6"),
  {ok, Fh} = file:open(FilePath, [read]),
  Lines = read_lines(Fh),
  ?assert(len(Lines) =:= 5).