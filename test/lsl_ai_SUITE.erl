-module(lsl_ai_SUITE).
-author('elbrujohalcon@inaka.net').

-export([ all/0
        , dumb_ai/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec dumb_ai(lsl_test_utils:config()) -> {comment, []}.
dumb_ai(_Config) ->
  ct:comment("A new match is created"),
  Match0 = lsl_match:new(3),

  ct:comment("DumbAI crosses the first clean stick"),
  {next, Match1} = lsl_ai_dumb:play(Match0),
  [[x], [i, i], [i, i, i]] = lsl_match:snapshot(Match1),

  ct:comment("DumbAI crosses the first clean stick (now on row 2)"),
  {next, Match2} = lsl_ai_dumb:play(Match1),
  [[x], [x, i], [i, i, i]] = lsl_match:snapshot(Match2),

  ct:comment("DumbAI crosses the first clean stick (now on row 3)"),
  {next, Match30} = lsl_match:cross(2, 2, 1, Match2),
  {next, Match31} = lsl_match:cross(3, 1, 1, Match30),
  {won, Match3} = lsl_ai_dumb:play(Match31),
  [[x], [x, x], [x, x, i]] = lsl_match:snapshot(Match3),

  ct:comment("DumbAI throws cant_move if board is full"),
  {lost, Match4} = lsl_match:cross(3, 3, 1, Match3),
  try lsl_ai_dumb:play(Match4) of
    Result -> ct:fail("Unexpected result: ~p", [Result])
  catch
    _:cant_move -> ok
  end,

  {comment, ""}.
