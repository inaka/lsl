%%% @doc Matches repository
-module(lsl_matches_repo).
-author('elbrujohalcon@inaka.net').

-export([ start/3
        , find/2
        , fetch/1
        , play/5
        , is_match/1
        , is_playing/2
        , is_current_player/2
        , stop/1
        ]).

%% @doc Starts a match.
%%      If the rival is AI, plays the first move.
-spec start(binary(), module()|binary(), pos_integer()) ->
  lsl_matches:match().
start(PlayerId, Rival, Rows) when is_binary(Rival) ->
  Match = lsl_matches:new(PlayerId, player, Rival, lsl_core:new(Rows)),
  sumo:persist(lsl_matches, Match);
start(PlayerId, Rival, Rows) when is_atom(Rival) ->
  Match = lsl_matches:new(PlayerId, ai, Rival, lsl_core:new(Rows)),
  {next, Core} = lsl_ai:play(Rival, lsl_matches:core(Match)),
  sumo:persist(lsl_matches, lsl_matches:core(Match, Core)).

%% @doc Retrieves a list of matches for a particular player
-spec find(lsl_players:player(), all | lsl_matches:status()) ->
  [lsl_matches:match()].
find(Player, Status) ->
  PlayerId = lsl_players:id(Player),
  Conditions = {'or', [{player_id, PlayerId}, {rival, PlayerId}]},
  AllMatches = sumo:find_by(lsl_matches, Conditions),
  filter(AllMatches, PlayerId, Status).

filter(AllMatches, _PlayerId, all) -> AllMatches;
filter(AllMatches, PlayerId, Status) ->
  [Match || Match <- AllMatches, lsl_matches:status(Match, PlayerId) == Status].

%% @doc Retrieves a match
-spec fetch(binary()) -> notfound | lsl_matches:match().
fetch(MatchId) -> sumo:find(lsl_matches, MatchId).

%% @doc Makes a move in a game
-spec play(
  binary(), binary(), lsl_core:row(), lsl_core:col(), lsl_core:length()) ->
  lsl_matches:match().
play(MatchId, PlayerId, Row, Col, Length) ->
  case sumo:find(lsl_matches, MatchId) of
    notfound -> throw(notfound);
    Match ->
      case lsl_matches:current_player(Match) of
        PlayerId -> do_play(Match, PlayerId, Row, Col, Length);
        _ -> throw(invalid_player)
      end
  end.
do_play(Match, _PlayerId, Row, Col, Length) ->
  NewCore =
    case lsl_core:cross(Row, Col, Length, lsl_matches:core(Match)) of
      {lost, Core} -> Core;
      {_, Core} ->
        case lsl_matches:rival_kind(Match) of
          player -> Core;
          ai ->
            {_, Core2} = lsl_ai:play(lsl_matches:rival(Match), Core),
            Core2
        end
    end,
  sumo:persist(lsl_matches, lsl_matches:core(Match, NewCore)).


%% @doc Is this a valid match id?
-spec is_match(binary()) -> boolean().
is_match(MatchId) -> notfound =/= sumo:find(lsl_matches, MatchId).

%% @doc Is the player playing that game
-spec is_playing(binary(), binary()) -> boolean().
is_playing(MatchId, PlayerId) ->
  case sumo:find(lsl_matches, MatchId) of
    notfound -> false;
    Match -> lists:member(PlayerId, lsl_matches:players(Match))
  end.

%% @doc Is the player the current player for that game
-spec is_current_player(binary(), binary()) -> boolean().
is_current_player(MatchId, PlayerId) ->
  case sumo:find(lsl_matches, MatchId) of
    notfound -> false;
    Match -> PlayerId == lsl_matches:current_player(Match)
  end.

%% @doc Deletes a match
-spec stop(binary()) -> boolean().
stop(MatchId) -> sumo:delete(lsl_matches, MatchId).
