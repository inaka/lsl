-module(lsl_app_SUITE).
-author('elbrujohalcon@inaka.net').

-ignore_xref([all/0, init_per_testcase/2]).
-ignore_xref([app_starts/1, app_stops/1]).

-export([all/0, init_per_testcase/2]).
-export([app_starts/1, app_stops/1]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
  lsl_test_utils:config().
init_per_testcase(app_starts, Config) ->
  ct:comment("The app should not be running"),
  _ = application:stop(lsl),
  [] = [Vsn || {lsl, _, Vsn} <- application:which_applications()],
  Config;
init_per_testcase(app_stops, Config) ->
  ct:comment("The app should be running"),
  {ok, _} = application:ensure_all_started(lsl),
  [_] = [Vsn || {lsl, _, Vsn} <- application:which_applications()],
  Config.

-spec app_starts(lsl_test_utils:config()) -> {comment, []}.
app_starts(_Config) ->
  ct:comment("After starting, the app should be running"),
  {ok, [_|_]} = lsl:start(),
  [_] = [Vsn || {lsl, _, Vsn} <- application:which_applications()],
  {comment, ""}.

-spec app_stops(lsl_test_utils:config()) -> {comment, []}.
app_stops(_Config) ->
  ct:comment("After stopping, the app should not be running"),
  ok = lsl:stop(),
  [] = [Vsn || {lsl, _, Vsn} <- application:which_applications()],
  {comment, ""}.
