%%% @doc Rodolfo AI for LSL.
%%%      Tries to keep an even number of stick groups,
%%%      - if there are even groups and some lone sticks, he crosses a stick
%%%      - if there are even groups and no sticks, he reduces the first of the
%%%        largest groups
%%%      - if there are odd groups, he crosses the first of the smallest groups
%%%      - if there is only one group, he does what he needs to do to leave odd
%%%        sticks
%%% @end
-module(lsl_ai_rodolfo).
-author('elbrujohalcon@inaka.net').

-behaviour(lsl_ai).

-export([play/1, name/0, next_move/1]).

%% @equiv lsl_ai:play(lsl_ai_dumb, Match).
-spec play(lsl_core:match()) -> {lsl_core:cross_result(), lsl_core:match()}.
play(Match) -> lsl_ai:play(?MODULE, Match).

%% @private
-spec name() -> binary().
name() -> <<"DumbAI">>.

%% @private
-spec next_move(lsl_core:match()) -> lsl_ai:move().
next_move(Match) ->
  Snapshot = lsl_core:snapshot(Match),
  case parse(Snapshot) of
    % No groups => cross stick
    {[Stick|_], []} -> cross(Stick);
    % One group, even sticks => turn group into stick
    {Sticks, [Group]} when length(Sticks) rem 2 =:= 0 -> leave_one(Group);
    % One group, odd sticks => remove group
    {_Sticks, [Group]} -> cross(Group);
    % Odd groups => cross smallest one
    {_Sticks, [Group|Groups]} when length(Groups) rem 2 =:= 0 -> cross(Group);
    % Even groups, no sticks => reduce largest one
    {[], Groups} -> reduce(hd(lists:reverse(Groups)));
    % Even groups, at least one stick => cross stick
    {[Stick|_Sticks], _Groups} -> cross(Stick)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(Snapshot) ->
  parse(Snapshot, 1, 1, 0, []).

parse([], _Row, _Col, _Length, Moves) ->
  SortedMoves = lists:keysort(3, lists:sort(Moves)),
  lists:splitwith(fun({_, _, Length}) -> Length == 1 end, SortedMoves);
parse([[] | Rows], Row, _Col, 0, Moves) ->
  parse(Rows, Row + 1, 1, 0, Moves);
parse([[] | Rows], Row, Col, Length, Moves) ->
  parse(Rows, Row + 1, 1, 0, [{Row, Col, Length} | Moves]);
parse([[x | Cells] | Rows], Row, Col, 0, Moves) ->
  parse([Cells|Rows], Row, Col + 1, 0, Moves);
parse([[x | Cells] | Rows], Row, Col, Length, Moves) ->
  parse([Cells|Rows], Row, Col + Length, 0, [{Row, Col, Length} | Moves]);
parse([[i | Cells] | Rows], Row, Col, Length, Moves) ->
  parse([Cells|Rows], Row, Col, Length + 1, Moves).

cross(Move) -> Move.

leave_one({Row, Col, Length}) -> {Row, Col, Length - 1}.

reduce({Row, Col, _Length}) -> {Row, Col, 1}.
