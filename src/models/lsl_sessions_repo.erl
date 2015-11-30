%%% @doc Sessions repository
-module(lsl_sessions_repo).
-author('elbrujohalcon@inaka.net').

-export([ can_close/2
        , close/1
        , fetch/2
        ]).

%% @doc Retrieves a session by its token and secret
-spec fetch(binary(), binary()) -> lsl_sessions:session() | notfound.
fetch(Token, Secret) ->
  case sumo:find(lsl_sessions, Token) of
    notfound -> notfound;
    Session ->
      case lsl_sessions:secret(Session) of
        Secret -> Session;
        _ -> notfound
      end
  end.

%% @doc Removes a session
-spec close(binary()) -> boolean().
close(Token) ->
  sumo:delete(lsl_sessions, Token).

%% @doc Is the player allowed to close the session?
-spec can_close(binary(), binary()) -> boolean().
can_close(PlayerId, Token) ->
  case sumo:find(lsl_sessions, Token) of
    notfound -> true;
    Session -> PlayerId =:= lsl_sessions:player_id(Session)
  end.
