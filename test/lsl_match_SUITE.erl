-module(lsl_match_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([ invalid_board/1
        , valid_board/1
        , play_out_of_bounds/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec invalid_board(lsl_test_utils:config()) -> {comment, []}.
invalid_board(_Config) ->
  ct:comment("Can't build a board without rows"),
  try lsl_match:new(0) of
    Match1 -> ct:fail("Unexpected Match: ~p", [Match1])
  catch
    _:invalid_board -> ok
  end,
  ct:comment("Can't build a board with negative rows"),
  try lsl_match:new(-1) of
    Match2 -> ct:fail("Unexpected Match: ~p", [Match2])
  catch
    _:invalid_board -> ok
  end,
  ct:comment("Can't build a board with just one row"),
  try lsl_match:new(1) of
    Match3 -> ct:fail("Unexpected Match: ~p", [Match3])
  catch
    _:invalid_board -> ok
  end,
  {comment, ""}.

-spec valid_board(lsl_test_utils:config()) -> {comment, []}.
valid_board(_Config) ->
  ct:comment("A match with 2 rows starts with a clean 2-row board"),
  Match = lsl_match:new(2),
  2 = lsl_match:rows(Match),
  [[i], [i, i]] = lsl_match:snapshot(Match),

  ct:comment("A match with 5 rows starts with a clean 5-row board"),
  Match2 = lsl_match:new(5),
  5 = lsl_match:rows(Match2),
  [[i], [i, i], [i, i, i], [i, i, i, i], [i, i, i, i ,i]] =
    lsl_match:snapshot(Match2),

  {comment, ""}.

-spec play_out_of_bounds(lsl_test_utils:config()) -> {comment, []}.
play_out_of_bounds(_Config) ->
  ct:comment("A new match is created"),
  Match = lsl_match:new(3),

  ShouldFail =
    fun(Row, ColStart, ColEnd, Match) ->
      try lsl_match:cross(Row, ColStart, ColEnd, Match) of
        NewMatch -> ct:fail("Unexpected Match: ~p", [Match])
      catch
        _:out_of_bounds -> ok
      end
    end,

  ct:comment("Crossing a row < 1 is invalid"),
  ShouldFail(0, 1, 1, Match),
  ShouldFail(0, 2, 1, Match),

  ct:comment("Crossing a row > Rows is invalid"),
  ShouldFail(4, 1, 1, Match),
  ShouldFail(4, 2, 2, Match),

  ct:comment("Crossing a starting col < 1 is invalid"),
  ShouldFail(1, 0, 1, Match),
  ShouldFail(2, 0, 0, Match),

  ct:comment("Crossing a starting col > ending col is invalid"),
  ShouldFail(3, 2, 1, Match),
  ShouldFail(2, 2, 1, Match),

  ct:comment("Crossing a starting col > row is invalid"),
  ShouldFail(1, 10, 11, Match),
  ShouldFail(1, 2, 2, Match),
  ShouldFail(2, 3, 3, Match),

  ct:comment("Crossing an ending col > row is invalid"),
  ShouldFail(1, 1, 2, Match),
  ShouldFail(2, 1, 3, Match),
  ShouldFail(2, 2, 35, Match),

  {comment, ""}.
