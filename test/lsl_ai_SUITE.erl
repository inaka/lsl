-module(lsl_ai_SUITE).
-author('elbrujohalcon@inaka.net').
-ignore_xref([ all/0
             , dumb_ai/1, dumb_ai_full_match/1
             , rodolfo_start/1, rodolfo_finish/1, rodolfo_full_match/1
             ]).

-export([ all/0
        , dumb_ai/1, dumb_ai_full_match/1
        , rodolfo_start/1, rodolfo_finish/1, rodolfo_full_match/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec dumb_ai(lsl_test_utils:config()) -> {comment, []}.
dumb_ai(_Config) ->
  ct:comment("A new match is created"),
  Match0 = lsl_core:new(3),

  ct:comment("DumbAI crosses the first clean stick"),
  {next, Match1} = lsl_ai_dumb:play(Match0),
  [[x], [i, i], [i, i, i]] = lsl_core:snapshot(Match1),

  ct:comment("DumbAI crosses the first clean stick (now on row 2)"),
  {next, Match2} = lsl_ai_dumb:play(Match1),
  [[x], [x, i], [i, i, i]] = lsl_core:snapshot(Match2),

  ct:comment("DumbAI crosses the first clean stick (now on row 3)"),
  {next, Match30} = lsl_core:cross(2, 2, 1, Match2),
  {next, Match31} = lsl_core:cross(3, 1, 1, Match30),
  {won, Match3} = lsl_ai_dumb:play(Match31),
  [[x], [x, x], [x, x, i]] = lsl_core:snapshot(Match3),

  ct:comment("DumbAI throws cant_move if board is full"),
  {lost, Match4} = lsl_core:cross(3, 3, 1, Match3),
  try lsl_ai_dumb:play(Match4) of
    Result -> ct:fail("Unexpected result: ~p", [Result])
  catch
    _:cant_move -> ok
  end,

  {comment, ""}.

-spec dumb_ai_full_match(lsl_test_utils:config()) -> {comment, []}.
dumb_ai_full_match(_Config) ->
  lsl_test_utils:full_match(lsl_ai_dumb, lsl_core:new(2)),
  lsl_test_utils:full_match(lsl_ai_dumb, lsl_core:new(5)),
  lsl_test_utils:full_match(lsl_ai_dumb, lsl_core:new(10)),
  lsl_test_utils:full_match(lsl_ai_dumb, lsl_core:new(20)),
  {comment, ""}.

-spec rodolfo_start(lsl_test_utils:config()) -> {comment, []}.
rodolfo_start(_Config) ->
  ct:comment("A new match is created"),
  Match0 = lsl_core:new(5),

  ct:comment(
    "Rodolfo will always try to leave a pair # of groups."
    "Since there are 4 groups and 1 loose stick, he will cross it"),
  {next, Match1} = lsl_ai_rodolfo:play(Match0),
  [[x], [i, i], [i, i, i], [i, i, i, i], [i, i, i, i, i]] =
    lsl_core:snapshot(Match1),

  ct:comment(
    "Since there are no loose sticks, he will reduce the largest group by one"),
  {next, Match2} = lsl_ai_rodolfo:play(Match1),
  [[x], [i, i], [i, i, i], [i, i, i, i], [x, i, i, i, i]] =
    lsl_core:snapshot(Match2),

  ct:comment(
    "Since there are 2 largest groups, he will reduce the last one by one"),
  {next, Match3} = lsl_ai_rodolfo:play(Match2),
  [[x], [i, i], [i, i, i], [i, i, i, i], [x, x, i, i, i]] =
    lsl_core:snapshot(Match3),

  ct:comment(
    "When there is an odd # of groups, he will remove the smallest one"),
  {next, Match4A0} = lsl_core:cross(5, 3, 3, Match3),
  {next, Match4A} = lsl_ai_rodolfo:play(Match4A0),
  [[x], [x, x], [i, i, i], [i, i, i, i], [x, x, x, x, x]] =
    lsl_core:snapshot(Match4A),

  ct:comment(
    "When there are 2 smallest groups, he will remove the first one"),
  {next, Match4B0} = lsl_core:cross(5, 3, 3, Match3),
  {next, Match4B1} = lsl_core:cross(3, 1, 1, Match4B0),
  {next, Match4B} = lsl_ai_rodolfo:play(Match4B1),
  [[x], [x, x], [x, i, i], [i, i, i, i], [x, x, x, x, x]] =
    lsl_core:snapshot(Match4B),

  ct:comment("Rodolfo can detect multiple groups in a row"),
  Match50 = lsl_core:new(5),
  {next, Match51} = lsl_core:cross(5, 3, 1, Match50),
  {next, Match5} = lsl_ai_rodolfo:play(Match51),
  [[i], [x, x], [i, i, i], [i, i, i, i], [i, i, x, i, i]] =
    lsl_core:snapshot(Match5),

  ct:comment("Rodolfo can detect a group inside a row"),
  Match60 = lsl_core:new(5),
  {next, Match61} = lsl_core:cross(2, 1, 2, Match60),
  {next, Match62} = lsl_core:cross(4, 1, 1, Match61),
  {next, Match63} = lsl_core:cross(4, 4, 1, Match62),
  {next, Match6} = lsl_ai_rodolfo:play(Match63),
  [[i], [x, x], [i, i, i], [x, x, x, x], [i, i, i, i, i]] =
    lsl_core:snapshot(Match6),

  {comment, ""}.

-spec rodolfo_finish(lsl_test_utils:config()) -> {comment, []}.
rodolfo_finish(_Config) ->
  ct:comment("A new match is created"),
  Match0 = lsl_core:new(3),

  ct:comment(
    "Rodolfo will always try to leave a pair # of groups."
    "Since there are 2 groups and 1 loose stick, he will cross it"),
  {next, Match1} = lsl_ai_rodolfo:play(Match0),
  [[x], [i, i], [i, i, i]] = lsl_core:snapshot(Match1),

  ct:comment("Now, with just 2 groups, he will reduce the longest one"),
  {next, Match2} = lsl_ai_rodolfo:play(Match1),
  [[x], [i, i], [x, i, i]] = lsl_core:snapshot(Match2),

  ct:comment("Now, with just 2 groups, he will reduce the first one"),
  {next, Match3} = lsl_ai_rodolfo:play(Match2),
  [[x], [i, i], [x, x, i]] = lsl_core:snapshot(Match3),

  ct:comment("Now that he has a chance to win, he'll take it"),
  {won, Match4} = lsl_ai_rodolfo:play(Match3),
  [[x], [x, x], [x, x, i]] = lsl_core:snapshot(Match4),

  ct:comment("He will take it also when it means crossing a single stick"),
  Match50 = lsl_core:new(3),
  {next, Match51} = lsl_core:cross(3, 2, 2, Match50),
  {next, Match5} = lsl_ai_rodolfo:play(Match51),
  [[i], [x, i], [i, x, x]] = lsl_core:snapshot(Match5),

  {next, Match6} = lsl_ai_rodolfo:play(Match5),
  [[x], [x, i], [i, x, x]] = lsl_core:snapshot(Match6),

  {won, Match7} = lsl_ai_rodolfo:play(Match6),
  [[x], [x, x], [i, x, x]] = lsl_core:snapshot(Match7),

  {comment, ""}.

-spec rodolfo_full_match(lsl_test_utils:config()) -> {comment, []}.
rodolfo_full_match(_Config) ->
  lsl_test_utils:full_match(lsl_ai_rodolfo, lsl_core:new(2)),
  lsl_test_utils:full_match(lsl_ai_rodolfo, lsl_core:new(5)),
  lsl_test_utils:full_match(lsl_ai_rodolfo, lsl_core:new(10)),
  lsl_test_utils:full_match(lsl_ai_rodolfo, lsl_core:new(20)),
  {comment, ""}.

