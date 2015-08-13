-module(lsl_core_SUITE).
-author('elbrujohalcon@inaka.net').

-ignore_xref([all/0]).
-ignore_xref([ invalid_board/1
             , valid_board/1
             , play_out_of_bounds/1
             , play_in_bounds/1
             , cant_cross_crossed/1
             , with_just_one_left_won/1
             , with_no_sticks_left_lost/1
             , no_undo/1
             , undo/1
             , pretty_print/1
             ]).
-export([all/0]).
-export([ invalid_board/1
        , valid_board/1
        , play_out_of_bounds/1
        , play_in_bounds/1
        , cant_cross_crossed/1
        , with_just_one_left_won/1
        , with_no_sticks_left_lost/1
        , no_undo/1
        , undo/1
        , pretty_print/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec invalid_board(lsl_test_utils:config()) -> {comment, []}.
invalid_board(_Config) ->
  ct:comment("Can't build a board without rows"),
  try lsl_core:new(0) of
    Match1 -> ct:fail("Unexpected Match: ~p", [Match1])
  catch
    _:invalid_board -> ok
  end,
  ct:comment("Can't build a board with negative rows"),
  try lsl_core:new(-1) of
    Match2 -> ct:fail("Unexpected Match: ~p", [Match2])
  catch
    _:invalid_board -> ok
  end,
  ct:comment("Can't build a board with just one row"),
  try lsl_core:new(1) of
    Match3 -> ct:fail("Unexpected Match: ~p", [Match3])
  catch
    _:invalid_board -> ok
  end,
  {comment, ""}.

-spec valid_board(lsl_test_utils:config()) -> {comment, []}.
valid_board(_Config) ->
  ct:comment("A match with 2 rows starts with a clean 2-row board"),
  Match = lsl_core:new(2),
  2 = lsl_core:rows(Match),
  [[i], [i, i]] = lsl_core:snapshot(Match),

  ct:comment("A match with 5 rows starts with a clean 5-row board"),
  Match2 = lsl_core:new(5),
  5 = lsl_core:rows(Match2),
  [[i], [i, i], [i, i, i], [i, i, i, i], [i, i, i, i , i]] =
    lsl_core:snapshot(Match2),

  {comment, ""}.

-spec play_out_of_bounds(lsl_test_utils:config()) -> {comment, []}.
play_out_of_bounds(_Config) ->
  ct:comment("A new match is created"),
  Match = lsl_core:new(3),

  ShouldFail =
    fun(Row, ColStart, Length, TheMatch) ->
      try lsl_core:cross(Row, ColStart, Length, TheMatch) of
        {_, NewMatch} -> ct:fail("Unexpected Match: ~p", [NewMatch])
      catch
        _:out_of_bounds -> ok
      end
    end,

  ct:comment("Crossing a row < 1 is invalid"),
  ShouldFail(0, 1, 1, Match),
  ShouldFail(0, 2, 1, Match),

  ct:comment("Crossing a row > Rows is invalid"),
  ShouldFail(4, 1, 1, Match),
  ShouldFail(4, 2, 1, Match),

  ct:comment("Crossing a col < 1 is invalid"),
  ShouldFail(1, 0, 1, Match),
  ShouldFail(2, 0, 0, Match),

  ct:comment("Crossing a length < 1 is invalid"),
  ShouldFail(3, 2, 0, Match),
  ShouldFail(2, 2, -1, Match),

  ct:comment("Crossing a starting col > row is invalid"),
  ShouldFail(1, 10, 1, Match),
  ShouldFail(1, 2, 1, Match),
  ShouldFail(2, 3, 1, Match),

  ct:comment("Crossing a length that goes beyond row is invalid"),
  ShouldFail(1, 1, 2, Match),
  ShouldFail(2, 1, 3, Match),
  ShouldFail(2, 2, 2, Match),

  {comment, ""}.

-spec play_in_bounds(lsl_test_utils:config()) -> {comment, []}.
play_in_bounds(_Config) ->
  ct:comment("A new match is created"),
  Match = lsl_core:new(3),
  [[i], [i, i], [i, i, i]] = lsl_core:snapshot(Match),

  CrossAndShot =
    fun(Row, Col, Length, TheMatch) ->
      {_, NewMatch} = lsl_core:cross(Row, Col, Length, TheMatch),
      lsl_core:snapshot(NewMatch)
    end,

  ct:comment("Playing in the first row, crosses its only element"),
  [[x], [i, i], [i, i, i]] = CrossAndShot(1, 1, 1, Match),

  ct:comment("Playing in the second row, crosses the proper elements"),
  [[i], [x, i], [i, i, i]] = CrossAndShot(2, 1, 1, Match),
  [[i], [x, x], [i, i, i]] = CrossAndShot(2, 1, 2, Match),
  [[i], [i, x], [i, i, i]] = CrossAndShot(2, 2, 1, Match),

  ct:comment("Playing in the third row, crosses the proper elements"),
  [[i], [i, i], [x, i, i]] = CrossAndShot(3, 1, 1, Match),
  [[i], [i, i], [i, x, i]] = CrossAndShot(3, 2, 1, Match),
  [[i], [i, i], [i, i, x]] = CrossAndShot(3, 3, 1, Match),
  [[i], [i, i], [x, x, i]] = CrossAndShot(3, 1, 2, Match),
  [[i], [i, i], [i, x, x]] = CrossAndShot(3, 2, 2, Match),
  [[i], [i, i], [x, x, x]] = CrossAndShot(3, 1, 3, Match),

  {comment, ""}.

-spec cant_cross_crossed(lsl_test_utils:config()) -> {comment, []}.
cant_cross_crossed(_Config) ->
  ct:comment("A new match is created and two tiles are crossed in row 5"),
  EmptyMatch = lsl_core:new(5),
  {next, Match0} = lsl_core:cross(5, 2, 1, EmptyMatch),
  {next, Match} = lsl_core:cross(5, 4, 1, Match0),

  ShouldFail =
    fun(Row, ColStart, Length, TheMatch) ->
      try lsl_core:cross(Row, ColStart, Length, TheMatch) of
        {_, NewMatch} -> ct:fail("Unexpected Match: ~p", [NewMatch])
      catch
        _:already_crossed -> ok
      end
    end,

  ct:comment("Can't just cross a crossed stick"),
  ShouldFail(5, 2, 1, Match),
  ShouldFail(5, 4, 1, Match),

  ct:comment("Can't cross to the left of a crossed stick"),
  ShouldFail(5, 1, 2, Match),
  ShouldFail(5, 3, 2, Match),
  ShouldFail(5, 1, 4, Match),

  ct:comment("Can't cross to the right of a crossed stick"),
  ShouldFail(5, 2, 2, Match),
  ShouldFail(5, 4, 2, Match),
  ShouldFail(5, 2, 4, Match),

  ct:comment("Can't cross a line with crossed sticks"),
  ShouldFail(5, 2, 3, Match),
  ShouldFail(5, 1, 5, Match),

  {comment, ""}.

-spec with_just_one_left_won(lsl_test_utils:config()) -> {comment, []}.
with_just_one_left_won(_Config) ->
  ct:comment("A new match is created"),
  Match = lsl_core:new(2),

  ct:comment("Player left just one stick and wins"),
  {won, _} = lsl_core:cross(2, 1, 2, Match),
  {comment, ""}.

-spec with_no_sticks_left_lost(lsl_test_utils:config()) -> {comment, []}.
with_no_sticks_left_lost(_Config) ->
  ct:comment("A new match is created and first player crosses the first row"),
  EmptyMatch = lsl_core:new(2),
  {next, Match} = lsl_core:cross(1, 1, 1, EmptyMatch),

  ct:comment("Player crosses the whole second row and loses"),
  {lost, _} = lsl_core:cross(2, 1, 2, Match),
  {comment, ""}.

-spec no_undo(lsl_test_utils:config()) -> {comment, []}.
no_undo(_Config) ->
  ct:comment("A new match is created"),
  Match = lsl_core:new(2),

  ct:comment("No undo is possible"),
  try lsl_core:undo(Match) of
    BadMatch -> ct:fail("Unexpected match: ~p", [BadMatch])
  catch
    _:no_history -> ok
  end,

  {comment, ""}.

-spec undo(lsl_test_utils:config()) -> {comment, []}.
undo(_Config) ->
  ct:comment("A new match is created and a movement is made"),
  Match0 = lsl_core:new(3),
  {next, MatchA} = lsl_core:cross(2, 1, 1, Match0),
  [[i], [x, i], [i, i, i]] = lsl_core:snapshot(MatchA),

  ct:comment("The move is undone"),
  Match0 = lsl_core:undo(MatchA),

  ct:comment("2 moves are made"),
  {next, Match1} = lsl_core:cross(1, 1, 1, Match0),
  {next, Match2} = lsl_core:cross(2, 2, 1, Match1),
  [[x], [i, x], [i, i, i]] = lsl_core:snapshot(Match2),

  ct:comment("Last move is undone"),
  Match1 = lsl_core:undo(Match2),

  ct:comment("First move is undone"),
  Match0 = lsl_core:undo(Match1),

  {comment, ""}.

-spec pretty_print(lsl_test_utils:config()) -> {comment, []}.
pretty_print(_Config) ->
  Print =
    fun(Match) ->
      binary:split(
        iolist_to_binary(lsl_core:print(Match)), <<"\n">>, [global, trim])
    end,

  ct:comment("A new match with 2 rows should be correctly printed"),
  [<<" | ">>, <<"| |">>] = Print(lsl_core:new(2)),

  ct:comment("A new match with 3 rows should be correctly printed"),
  Match0 = lsl_core:new(3),
  [<<"  |  ">>, <<" | | ">>, <<"| | |">>] = Print(Match0),

  ct:comment("After crossing a stick, it should be crossed"),
  {next, Match1} = lsl_core:cross(2, 1, 1, Match0),
  [<<"  |  ">>, <<" + | ">>, <<"| | |">>] = Print(Match1),

  ct:comment("After crossing 2 adjacent sticks, they should be connected"),
  {next, Match2} = lsl_core:cross(2, 2, 1, Match1),
  [<<"  |  ">>, <<" +-+ ">>, <<"| | |">>] = Print(Match2),

  ct:comment("After crossing 1 stick out of 3, they should be connected"),
  {next, Match3} = lsl_core:cross(3, 1, 1, Match2),
  [<<"  |  ">>, <<" +-+ ">>, <<"+ | |">>] = Print(Match3),

  ct:comment("After crossing 3 adjacent sticks, they should be connected"),
  {won, Match4} = lsl_core:cross(3, 2, 2, Match3),
  [<<"  |  ">>, <<" +-+ ">>, <<"+-+-+">>] = Print(Match4),

  {comment, ""}.
