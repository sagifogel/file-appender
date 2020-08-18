-module(file_appender_tests).
-import(string, [len/1]).
-include_lib("eunit/include/eunit.hrl").

appending_lines_to_file_appender_causing_the_file_to_be_written_test() ->
  FilePath = "C:\\Users\\Home\\Desktop\\erlang\\file1.txt",
  file:delete(FilePath),
  Pid = file_appender:start(FilePath),
  file_appender:append_line(Pid, "1"),
  file_appender:append_line(Pid, "2"),
  file_appender:append_line(Pid, "3"),
  file_appender:append_line(Pid, "4"),
  file_appender:append_line(Pid, "5"),
  {ok, Fh} = file:open(FilePath, [read]),
  Lines = read_lines(Fh),
  ?assert(len(Lines) =:= 5).

appending_lines_to_file_appender_with_or_without_line_feed_returns_the_correct_number_of_inputs_test() ->
  FilePath = "C:\\Users\\Home\\Desktop\\erlang\\file2.txt",
  file:delete(FilePath),
  Pid = file_appender:start(FilePath),
  file_appender:append_line(Pid, "1\n"),
  file_appender:append_line(Pid, "2"),
  file_appender:append_line(Pid, "3\n"),
  file_appender:append_line(Pid, "4"),
  file_appender:append_line(Pid, "5\n"),
  {ok, Fh} = file:open(FilePath, [read]),
  Lines = read_lines(Fh),
  ?assert(len(Lines) =:= 5).

read_lines(Fn) ->
  case io:get_line(Fn, "") of
    eof -> [];
    Line -> [Line | read_lines(Fn)]
  end.