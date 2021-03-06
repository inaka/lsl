%%% @doc Internal representation and logic of a LSL match.
-module(lsl_core).
-author('elbrujohalcon@inaka.net').

-type stick_state() :: clean | crossed.
-type row_state()   :: [stick_state()].
-type board()       :: [row_state()].
-type history()     :: [row_state()].
-opaque match()     :: #{ board => board()
                        , history => history()
                        }.
-export_type([match/0]).

-type row()     :: pos_integer().
-type col()     :: pos_integer().
-type length()  :: pos_integer().
-export_type([row/0, col/0, length/0]).

-type stick_snapshot() :: i | x.
-type row_snapshot()   :: [stick_snapshot(), ...].
-type board_snapshot() :: [row_snapshot(), ...].
-export_type([stick_snapshot/0, row_snapshot/0, board_snapshot/0]).

-type cross_result() :: next | won | lost.
-export_type([cross_result/0]).

-export([new/1, rows/1, snapshot/1]).
-export([cross/4, undo/1, last_result/1, turns/1]).
-export([print/1, to_json/1]).

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
snapshot(#{board := Board}) -> do_snapshot(Board).

%% @doc Crosses a couple of adjacent sticks
-spec cross(row(), col(), length(), match()) -> {cross_result(), match()}.
cross(Row, Col, Length, Match) ->
  validate_bounds(Row, Col, Length, rows(Match)),
  #{board := Board, history := History} = Match,
  {Up, OldRow, Down} = split_board_at(Row, Board),
  {Left, Rest} = lists:split(Col - 1, OldRow),
  {ToCross, Right} = lists:split(Length, Rest),
  validate_not_crossed(ToCross),
  Middle = lists:duplicate(Length, crossed),
  NewRow = Left ++ Middle ++ Right,
  NewBoard = Up ++ [NewRow|Down],
  { cross_result(NewBoard)
  , Match#{board := NewBoard, history := [OldRow|History]}
  }.

%% @doc Undoes the last move
%% @throws no_history if there are no moves to undo
-spec undo(match()) -> match().
undo(Match = #{history := [Row|History]}) ->
  #{board := Board} = Match,
  RowNum = length(Row),
  {Up, _, Down} = split_board_at(RowNum, Board),
  NewBoard = Up ++ [Row|Down],
  Match#{board := NewBoard, history := History}.

%% @doc Returns the last cross result.
%%      In other words, the status of the match
-spec last_result(match()) -> cross_result().
last_result(#{board := Board}) ->
  cross_result(Board).

%% @doc How many turns have been played.
%%      In other words, the length of the history
-spec turns(match()) -> non_neg_integer().
turns(#{history := History}) -> length(History).

%% @doc returns a printable version of the board
-spec print(match()) -> iodata().
print(#{board := Board}) ->
  RowWidth = length(Board),
  [print(Row, RowWidth) || Row <- Board].

%% @doc returns a json-able version of the board
-spec to_json(match()) -> [[boolean()]].
to_json(#{board := Board}) ->
  [[Item == crossed || Item <- Row] || Row <- Board].

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

print(Row, RowWidth) ->
  Padding = lists:duplicate(RowWidth - length(Row), $\s),
  [Padding, do_print(Row, <<>>), Padding, $\n].

do_print([clean], Acc) -> <<Acc/binary, "|">>;
do_print([crossed], Acc) -> <<Acc/binary, "+">>;
do_print([crossed, clean], Acc) -> <<Acc/binary, "+ |">>;
do_print([clean | Sticks], Acc) ->
  do_print(Sticks, <<Acc/binary, "| ">>);
do_print([crossed, clean | Sticks], Acc) ->
  do_print(Sticks, <<Acc/binary, "+ | ">>);
do_print([crossed, crossed | Sticks], Acc) ->
  do_print([crossed | Sticks], <<Acc/binary, "+-">>).

do_snapshot(clean) -> i;
do_snapshot(crossed) -> x;
do_snapshot(List) -> [do_snapshot(Elem) || Elem <- List].

split_board_at(RowNum, Board) ->
  {Up, [Row|Down]} = lists:split(RowNum - 1, Board),
  {Up, Row, Down}.
