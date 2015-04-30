%%% @doc Internal representation and logic of a LSL match.
-module(lsl_match).
-author('elbrujohalcon@inaka.net').

-type stick_state() :: clean | crossed.
-type row_state()   :: [stick_state()].
-type board()       :: [row_state()].
-type history()     :: [board()].
-opaque match()     :: #{ board => board()
                        , history => history()
                        }.
-export_type([match/0]).

-type row()     :: pos_integer().
-type col()     :: pos_integer().
-type length()  :: pos_integer().
-export_type([row/0, col/0, length/0]).

-type stick_snapshot() :: i | x.
-type row_snapshot()   :: [stick_snapshot()].
-type board_snapshot() :: [row_snapshot()].
-export_type([stick_snapshot/0, row_snapshot/0, board_snapshot/0]).

-type cross_result() :: next | won | lost.
-export_type([cross_result/0]).

-export([new/1, rows/1, snapshot/1]).
-export([cross/4, undo/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generates a new match.
-spec new(pos_integer()) -> match().
new(Rows) when is_integer(Rows), Rows >= 2 ->
  Board = [lists:duplicate(Row, clean) || Row <- lists:seq(1, Rows)],
  #{board => Board, history => []};
new(_Rows) -> throw(invalid_board).

%% @doc Returns the number of rows for the match
-spec rows(match()) -> pos_integer().
rows(#{board := Board}) -> length(Board).

%% @doc Returns a snapshot of a match
-spec snapshot(match()) -> board_snapshot().
snapshot(#{board := Board}) -> snapshot(Board);
snapshot(clean) -> i;
snapshot(crossed) -> x;
snapshot(List) -> [snapshot(Elem) || Elem <- List].

%% @doc Crosses a couple of adjacent sticks
-spec cross(row(), col(), length(), match()) -> {cross_result(), match()}.
cross(Row, Col, Length, Match) ->
  validate_bounds(Row, Col, Length, rows(Match)),
  #{board := Board, history := History} = Match,
  {Up, [OldRow|Down]} = lists:split(Row - 1, Board),
  {Left, Rest} = lists:split(Col - 1, OldRow),
  {ToCross, Right} = lists:split(Length, Rest),
  validate_not_crossed(ToCross),
  Middle = lists:duplicate(Length, crossed),
  NewRow = Left ++ Middle ++ Right,
  NewBoard = Up ++ [NewRow|Down],
  { cross_result(NewBoard)
  , Match#{board := NewBoard, history := [Board|History]}
  }.

%% @doc Undoes the last move
%% @throws no_history if there are no moves to undo
-spec undo(match()) -> match().
undo(Match = #{history := [Board|History]}) ->
  Match#{board := Board, history := History};
undo(_Match) -> throw(no_history).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_bounds(Row, Col, Length, Rows)
  when Row > 0
     , Row =< Rows
     , Col > 0
     , Length > 0
     , Col =< Row
     , Col + Length - 1 =< Row -> ok;
validate_bounds(_Row, _ColStart, _ColEnd, _Rows) -> throw(out_of_bounds).

validate_not_crossed(Sticks) ->
  case lists:member(crossed, Sticks) of
    true -> throw(already_crossed);
    false -> ok
  end.

cross_result(Board) ->
  case [clean || Row <- Board, clean <- Row] of
    [] -> lost;
    [clean] -> won;
    [clean|_] -> next
  end.
