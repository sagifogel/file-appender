-module(utils).
-import(string, [concat/2]).
-import(lists, [keysearch/3]).
-export([read_lines/1, get_root_path/0]).

get_root_path() ->
  {ok, CurrentDir} = file:get_cwd(),
  ConfPath = concat(CurrentDir, "/resources/test.config"),
  {ok, Conf} = file:consult(ConfPath),
  {_, {_, RootPath}} = keysearch(test_file_path, 1, Conf),
  RootPath.

read_lines(Fn) ->
  case io:get_line(Fn, "") of
    eof -> [];
    Line -> [Line | read_lines(Fn)]
  end.