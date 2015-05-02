%%% @doc AI behaviour definition.
-module(lsl_ai).
-author('elbrujohalcon@inaka.net').

-type move() :: {lsl_match:row(), lsl_match:col(), lsl_match:length()}.
-export_type([move/0]).

-callback next_move(lsl_match:match()) -> move().

-export([play/2]).

%% @doc plays a round
-spec play(module(), lsl_match:match()) ->
  {lsl_match:cross_result(), lsl_match:match()}.
play(Mod, Match) ->
  {Row, Col, Length} = Mod:next_move(Match),
  lsl_match:cross(Row, Col, Length, Match).
