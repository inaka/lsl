%%% @doc Sessions repository
-module(lsl_sessions_repo).
-author('elbrujohalcon@inaka.net').

-export([ can_close/2
        , fetch_player/2
        ]).

%% @doc Is the player allowed to close the session?
-spec can_close(binary(), binary()) -> boolean().
can_close(PlayerId, Token) ->
  case sumo:fetch(lsl_sessions, Token) of
    notfound -> true;
    Session -> PlayerId =:= lsl_sessions:player_id(Session)
  end.

%% @doc Retrieves a player given the token and secret for the session
-spec fetch_player(binary(), binary()) ->
  lsl_players:player() | notfound.
fetch_player(Token, Secret) ->
  case fetch(Token, Secret) of
    notfound -> notfound;
    Session -> lsl_players_repo:fetch(lsl_sessions:player_id(Session))
  end.

fetch(Token, Secret) ->
  case sumo:fetch(lsl_sessions, Token) of
    notfound -> notfound;
    Session ->
      case lsl_sessions:secret(Session) of
        Secret -> Session;
        _ -> notfound
      end
  end.
