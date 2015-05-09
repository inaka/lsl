%%% @doc Sessions repository
-module(lsl_sessions_repo).
-author('elbrujohalcon@inaka.net').

-export([ open/1
        ]).

%% @doc Creates a new session
-spec open(binary()) -> {lsl_sessions:session()}.
open(PlayerId) ->
  Secret = clean(base64:encode(crypto:strong_rand_bytes(32))),
  SecretHash = erlpass:hash(Secret),
  Session = lsl_sessions:new(PlayerId, SecretHash),
  PersistedSession = sumo:persist(lsl_sessions, Session),
  lsl_sessions:secret(PersistedSession, Secret).

clean(Binary) ->
  binary:replace(Binary, [<<$/>>, <<$=>>, <<$+>>], <<>>, [global]).
