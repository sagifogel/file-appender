-module(file_appender_tests).
-import(string, [len/1]).
-import(utils, [read_lines/1, get_root_path/0]).
-compile(export_all).
-include("config.hrl").
-include_lib("eunit/include/eunit.hrl").

reading_missing_config_file_returns_default_config_test() ->
  Conf = file_appender:parse_conf("missing.config"),
  ?assert(Conf#config.termination_interval =:= 5000).

appending_lines_to_file_appender_causing_the_file_to_be_written_test() ->
  [timeout, 10, ?_assert(appending_lines_to_file_appender_causing_the_file_to_be_written() =:= 5)].

appending_lines_to_file_appender_causing_the_file_to_be_written() ->
  Path = get_root_path(),
  FilePath = Path ++ "/file1.txt",
  file:delete(FilePath),
  Pid = file_appender:start(FilePath),
  file_appender:append_line(Pid, "1"),
  file_appender:append_line(Pid, "2"),
  file_appender:append_line(Pid, "3"),
  file_appender:append_line(Pid, "4"),
  file_appender:append_line(Pid, "5"),
  {ok, Fh} = file:open(FilePath, [read]),
  read_lines(Fh).

appending_lines_to_file_appender_with_or_without_line_feed_returns_the_correct_number_of_inputs_test() ->
  [timeout, 10, ?_assert(appending_lines_to_file_appender_with_or_without_line_feed_returns_the_correct_number_of_inputs() =:= 5)].

appending_lines_to_file_appender_with_or_without_line_feed_returns_the_correct_number_of_inputs() ->
  Path = get_root_path(),
  FilePath = Path ++ "/file2.txt",
  file:delete(FilePath),
  Pid = file_appender:start(FilePath),
  file_appender:append_line(Pid, "1\n"),
  file_appender:append_line(Pid, "2"),
  file_appender:append_line(Pid, "3\n"),
  file_appender:append_line(Pid, "4"),
  file_appender:append_line(Pid, "5\n"),
  {ok, Fh} = file:open(FilePath, [read]),
  read_lines(Fh).

appending_line_to_file_appender_after_timeout_is_not_possible_test() ->
  [timeout, 10, ?_assert(appending_line_to_file_appender_after_timeout_is_not_possible() =:= 5)].

appending_line_to_file_appender_after_timeout_is_not_possible() ->
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
  read_lines(Fh).