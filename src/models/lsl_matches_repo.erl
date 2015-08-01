%%% @doc Matches repository
-module(lsl_matches_repo).
-author('elbrujohalcon@inaka.net').

-export([ start/3
        , find/2
        , play/5
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

%% @doc Makes a move in a game
-spec play(
  binary(), binary(), lsl_core:row(), lsl_core:col(), lsl_core:length()) ->
  lsl_matches:match().
play(MatchId, PlayerId, Row, Col, Length) ->
  case sumo:find(lsl_matches, MatchId) of
    notfound -> throw(notfound);
    Match ->
      case lsl_matches:current_player(Match) of
        PlayerId ->
          {_, Core} = lsl_core:cross(Row, Col, Length, lsl_matches:core(Match)),
          sumo:persist(lsl_matches, lsl_matches:core(Match, Core));
        _ -> throw(invalid_player)
      end
  end.
