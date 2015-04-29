%%% @doc Internal representation and logic of a LSL match.
-module(lsl_match).
-author('elbrujohalcon@inaka.net').

-opaque item_state()  :: clean | crossed.
-opaque row_state()   :: [item_state()].
-opaque board()       :: [row_state()].
-opaque match()       :: #{ board => board()
                          }.
-export_type([match/0]).

-type row() :: pos_integer().
-type col() :: pos_integer().
-export_type([row/0, col/0]).

-type item_snapshot()  :: i | x.
-type row_snapshot()   :: [item_snapshot()].
-type board_snapshot() :: [row_snapshot()].
-export_type([ item_snapshot/0, row_snapshot/0, board_snapshot/0]).

-export([new/1, rows/1, snapshot/1]).
-export([cross/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generates a new match.
-spec new(pos_integer()) -> match().
new(Rows) when is_integer(Rows), Rows >= 2 ->
  Board = [lists:duplicate(Row, clean) || Row <- lists:seq(1, Rows)],
  #{board => Board};
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
-spec cross(row(), col(), col(), match()) -> match().
cross(Row, ColStart, ColEnd, Match) ->
  validate_bounds(Row, ColStart, ColEnd, rows(Match)),
  Match.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_bounds(Row, ColStart, ColEnd, Rows)
  when Row > 0
     , Row =< Rows
     , ColStart > 0
     , ColStart =< ColEnd
     , ColStart =< Row
     , ColEnd =< Row -> ok;
validate_bounds(_Row, _ColStart, _ColEnd, _Rows) -> throw(out_of_bounds).
