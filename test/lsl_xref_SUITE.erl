-module(lsl_xref_SUITE).
-author('elbrujohalcon@inaka.net').

-ignore_xref([all/0]).
-ignore_xref([xref/1]).

-export([all/0]).
-export([xref/1]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec xref(lsl_test_utils:config()) -> {comment, []}.
xref(_Config) ->
  Ebin = filename:absname("../../ebin"),
  Tests = filename:dirname(code:which(?MODULE)),
  Dirs = [Ebin, Tests],
  [] = xref_runner:check(undefined_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(undefined_functions, #{dirs => Dirs}),
  [] = xref_runner:check(locals_not_used, #{dirs => Dirs}),
  [] = xref_runner:check(exports_not_used, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_functions, #{dirs => Dirs}),
  {comment, ""}.
