%%% @doc Sessions repository
-module(lsl_sessions_repo).
-author('elbrujohalcon@inaka.net').

-export([ open/1
        , can_close/2
        , close/1
        , fetch/2
        ]).

%% @doc Creates a new session
-spec open(binary()) -> lsl_sessions:session().
open(PlayerId) ->
  Secret = lsl_crypto:secret(),
  SecretHash = lsl_crypto:hash(Secret),
  Session = lsl_sessions:new(PlayerId, SecretHash),
  PersistedSession = sumo:persist(lsl_sessions, Session),
  lsl_sessions:secret(PersistedSession, Secret).

%% @doc Retrieves a session by its token and secret
-spec fetch(binary(), binary()) -> lsl_sessions:session() | notfound.
fetch(Token, Secret) ->
  case sumo:find(lsl_sessions, Token) of
    notfound -> notfound;
    Session ->
      SecretHash = lsl_sessions:secret_hash(Session),
      case lsl_crypto:match(Secret, SecretHash) of
        true -> Session;
        false -> notfound
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
