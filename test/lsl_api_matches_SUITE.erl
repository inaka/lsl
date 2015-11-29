-module(lsl_api_matches_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-ignore_xref([ all/0
             , init_per_suite/1
             , end_per_suite/1
             , init_per_testcase/2
             , end_per_testcase/2
             ]).
-ignore_xref([ post_matches_wrong/1
             , post_matches_ok/1
             , get_matches_wrong/1
             , get_matches_ok/1
             , get_match_wrong/1
             , get_match_ok/1
             , patch_match_wrong/1
             , patch_match_ok/1
             , delete_match_wrong/1
             , delete_match_ok/1
             ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ post_matches_wrong/1
        , post_matches_ok/1
        , get_matches_wrong/1
        , get_matches_ok/1
        , get_matches_status_ok/1
        , get_match_wrong/1
        , get_match_ok/1
        , patch_match_wrong/1
        , patch_match_ok/1
        , delete_match_wrong/1
        , delete_match_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
init_per_testcase(TestCase, Config) ->
  application:set_env(lsl, default_rows, 3),
  Name = atom_to_binary(TestCase, utf8),
  Player1 = lsl_test_utils:register_player(<<Name/binary, "-1">>),
  Player2 = lsl_test_utils:register_player(<<Name/binary, "-2">>),
  Session1 = lsl_test_utils:open_session(Player1),
  Session2 = lsl_test_utils:open_session(Player2),
  [ {player1, Player1}
  , {player2, Player2}
  , {session1, Session1}
  , {session2, Session2}
  | Config
  ].

-spec end_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
end_per_testcase(_TestCase, Config) ->
  {value, {player1, Player1}, Config1} = lists:keytake(player1, 1, Config),
  sumo:delete(lsl_players, lsl_players:id(Player1)),
  {value, {player2, Player2}, Config2} = lists:keytake(player2, 1, Config1),
  sumo:delete(lsl_players, lsl_players:id(Player2)),
  {value, {session1, Session1}, Config3} = lists:keytake(session1, 1, Config2),
  lsl:close_session(lsl_sessions:token(Session1)),
  {value, {session2, Session2}, Config4} = lists:keytake(session2, 1, Config3),
  lsl:close_session(lsl_sessions:token(Session2)),
  application:unset_env(lsl, default_rows),
  Config4.

-spec post_matches_wrong(lsl_test_utils:config()) -> {comment, []}.
post_matches_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),

  ct:comment("POST without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(post, "/matches"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("POST with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(post, "/matches", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("POST with usr/pwd fails"),
  Headers1 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(post, "/matches", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("POST with wrong token/secret fails"),
  Headers2 = #{basic_auth => {"a bad token", "a bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(post, "/matches", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("POST with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(post, "/matches", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("POST without content-type fails"),
  Headers4 = #{basic_auth => {Token, Secret}},
  #{status_code := 415} = lsl_test_utils:api_call(post, "/matches", Headers4),

  ct:comment("Something that's not json fails as well"),
  Headers5 = Headers4#{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = lsl_test_utils:api_call(post, "/matches", Headers5),

  ct:comment("Even with the right type"),
  Headers = Headers4#{<<"content-type">> => <<"application/json">>},
  #{status_code := 400} = lsl_test_utils:api_call(post, "/matches", Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{"),
  #{<<"error">> := <<"invalid json">>} = sr_json:decode(Body0),

  ct:comment("No rival fails"),
  #{status_code := 400,
           body := Body1} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{}"),
  #{<<"error">> := <<"missing field: rival">>} = sr_json:decode(Body1),

  ct:comment("Invalid rival fails"),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{\"rival\":\"false\"}"),
  #{<<"error">> := <<"invalid field: rival">>} = sr_json:decode(Body2),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{\"rival\":\"io\"}"),

  ct:comment("Invalid rows fails"),
  ReqBody3 = sr_json:encode(#{rival => lsl_ai_dumb, rows => -1}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody3),
  #{<<"error">> := <<"invalid field: rows">>} = sr_json:decode(Body3),

  ReqBody4 = sr_json:encode(#{rival => Rival, rows => 0}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody4),

  ReqBody5 = sr_json:encode(#{rival => lsl_ai_dumb, rows => false}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody5),

  ReqBody6 = sr_json:encode(#{rival => Rival, rows => 1}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody6),

  {comment, ""}.

-spec post_matches_ok(lsl_test_utils:config()) -> {comment, []}.
post_matches_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),
  RivalName = lsl_players:name(Player2),
  Headers =
    #{ basic_auth => {Token, Secret}
     , <<"content-type">> => <<"application/json">>
     },
  DumbAIName = lsl_ai_dumb:name(),
  Rodolfo = lsl_ai_rodolfo:name(),

  ct:comment("Start a match against another player, default # of rows"),
  ReqBody1 = sr_json:encode(#{rival => Rival}),
  #{status_code := 201,
           body := Body1} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody1),
  #{ <<"id">> := Id1
   , <<"rival">> := #{<<"id">> := Rival, <<"name">> := RivalName}
   , <<"board">> := [ [false]
                    , [false, false]
                    , [false, false, false]
                    ]
   , <<"current-player">> := Rival
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body1),

  ct:comment("Start a match against another player, 2 rows"),
  ReqBody2 = sr_json:encode(#{rival => Rival, rows => 2}),
  #{status_code := 201,
           body := Body2} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody2),
  #{ <<"id">> := Id2
   , <<"rival">> := #{<<"id">> := Rival, <<"name">> := RivalName}
   , <<"board">> := [ [false]
                    , [false, false]
                    ]
   , <<"current-player">> := Rival
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body2),
  case Id2 of
    Id1 -> ct:fail("Duplicated game");
    Id2 -> ok
  end,

  ct:comment("Start a match against AI, default # of rows"),
  ReqBody3 = sr_json:encode(#{rival => lsl_ai_dumb}),
  #{status_code := 201,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody3),
  #{ <<"id">> := Id3
   , <<"rival">> := #{<<"id">> := <<"lsl_ai_dumb">>, <<"name">> := DumbAIName}
   , <<"board">> := [ [true]
                    , [false, false]
                    , [false, false, false]
                    ]
   , <<"current-player">> := PlayerId
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body3),

  ct:comment("Start a match against AI player, 2 rows"),
  ReqBody4 = sr_json:encode(#{rival => lsl_ai_rodolfo}),
  #{status_code := 201,
           body := Body4} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody4),
  #{ <<"id">> := Id4
   , <<"rival">> := #{<<"id">> := <<"lsl_ai_rodolfo">>, <<"name">> := Rodolfo}
   , <<"board">> := [ [true]
                    , [false, false]
                    , [false, false, false]
                    ]
   , <<"current-player">> := PlayerId
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body4),
  case Id4 of
    Id3 -> ct:fail("Duplicated game");
    Id4 -> ok
  end,

  {comment, ""}.

-spec get_matches_wrong(lsl_test_utils:config()) -> {comment, []}.
get_matches_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),

  ct:comment("GET without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(get, "/matches"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("GET with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(get, "/matches", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("GET with usr/pwd fails"),
  Headers1 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(get, "/matches", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("GET with wrong token/secret fails"),
  Headers2 = #{basic_auth => {"a bad token", "a bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(get, "/matches", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("GET with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(get, "/matches", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("GET with wrong status fails"),
  Headers = #{basic_auth => {Token, Secret}},
  #{status_code := 400} =
    lsl_test_utils:api_call(get, "/matches?status=wrong", Headers),

  #{status_code := 400} =
    lsl_test_utils:api_call(get, "/matches?status", Headers),

  {comment, ""}.

-spec get_matches_ok(lsl_test_utils:config()) -> {comment, []}.
get_matches_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  Headers = #{basic_auth => {Token, Secret}},
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),
  RivalName = lsl_players:name(Player2),
  DumbAIName = lsl_ai_dumb:name(),

  ct:comment("GET /matches returns an empty list"),
  #{status_code := 200,
           body := EmptyBody} =
    lsl_test_utils:api_call(get, "/matches", Headers),
  [] = sr_json:decode(EmptyBody),

  ct:comment("A match that doesn't include player1 is created"),
  _ = lsl:start_match(Rival, lsl_ai_dumb, 5),

  ct:comment("GET /matches still returns an empty list"),
  #{status_code := 200,
           body := EmptyBody} =
    lsl_test_utils:api_call(get, "/matches", Headers),

  ct:comment("A match with player1 as player 1 is created"),
  M1Id = lsl_matches:id(lsl:start_match(PlayerId, lsl_ai_dumb, 3)),

  ct:comment("GET /matches returns that match"),
  #{status_code := 200,
           body := Body1} =
    lsl_test_utils:api_call(get, "/matches", Headers),
  [ #{ <<"id">> := M1Id
     , <<"rival">> := #{<<"id">> := <<"lsl_ai_dumb">>, <<"name">> := DumbAIName}
     , <<"board">> := [ [true]
                      , [false, false]
                      , [false, false, false]
                      ]
     , <<"current-player">> := PlayerId
     , <<"status">> := <<"playing">>
     } = Match1
  ] = sr_json:decode(Body1),

  ct:comment("A match with player1 as rival is created"),
  M2Id = lsl_matches:id(lsl:start_match(Rival, PlayerId, 3)),

  ct:comment("GET /matches returns both matches"),
  #{status_code := 200,
           body := Body2} =
    lsl_test_utils:api_call(get, "/matches", Headers),
  [ #{ <<"id">> := M2Id
     , <<"rival">> := #{<<"id">> := Rival, <<"name">> := RivalName}
     , <<"board">> := [ [false]
                      , [false, false]
                      , [false, false, false]
                      ]
     , <<"current-player">> := PlayerId
     , <<"status">> := <<"playing">>
     }
  ] = sr_json:decode(Body2) -- [Match1],

  {comment, ""}.

-spec get_matches_status_ok(lsl_test_utils:config()) -> {comment, []}.
get_matches_status_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  Headers = #{basic_auth => {Token, Secret}},
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),
  {session2, SessionB} = lists:keyfind(session2, 1, Config),
  RivToken = binary_to_list(lsl_sessions:token(SessionB)),
  RivSecret = binary_to_list(lsl_sessions:secret(SessionB)),
  RivHeaders = #{basic_auth => {RivToken, RivSecret}},

  ct:comment("Three matches are created"),
  PlayingId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  WonId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  LostId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  AllIds = lists:sort([PlayingId, WonId, LostId]),

  ct:comment("Player1 wins one, looses 1"),
  _ = lsl:play(WonId, Rival, 3, 1, 3),
  _ = lsl:play(WonId, PlayerId, 2, 1, 2),
  _ = lsl:play(LostId, Rival, 3, 1, 3),
  _ = lsl:play(LostId, PlayerId, 1, 1, 1),
  _ = lsl:play(LostId, Rival, 2, 1, 1),

  MatchIds =
    fun(Status, Hs) ->
      #{status_code := 200,
               body := Body} =
        lsl_test_utils:api_call(get, "/matches?status=" ++ Status, Hs),
      lists:sort([MatchId || #{<<"id">> := MatchId} <- sr_json:decode(Body)])
    end,

  ct:comment("GET /matches?status=all returns all of them"),
  AllIds = MatchIds("all", Headers),
  AllIds = MatchIds("all", RivHeaders),

  ct:comment("GET /matches?status=won returns the on that the player won"),
  [WonId] = MatchIds("won", Headers),
  [LostId] = MatchIds("won", RivHeaders),

  ct:comment("GET /matches?status=lost returns the on that the player lost"),
  [LostId] = MatchIds("lost", Headers),
  [WonId] = MatchIds("lost", RivHeaders),

  ct:comment("everything still works *after* the games are absolutely over"),
  _ = lsl:play(WonId, Rival, 1, 1, 1),
  _ = lsl:play(LostId, PlayerId, 2, 2, 1),
  [WonId] = MatchIds("won", Headers),
  [LostId] = MatchIds("won", RivHeaders),
  [LostId] = MatchIds("lost", Headers),
  [WonId] = MatchIds("lost", RivHeaders),

  ct:comment("GET /matches?status=playing returns the on that's still ongoing"),
  [PlayingId] = MatchIds("playing", Headers),
  [PlayingId] = MatchIds("playing", RivHeaders),

  {comment, ""}.

-spec get_match_wrong(lsl_test_utils:config()) -> {comment, []}.
get_match_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),

  ct:comment("GET without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(get, "/matches/id"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("GET with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(get, "/matches/id", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("GET with usr/pwd fails"),
  Headers1 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(get, "/matches/id", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("GET with wrong token/secret fails"),
  Headers2 = #{basic_auth => {"a bad token", "a bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(get, "/matches/id", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("GET with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(get, "/matches/id", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("GET with wrong match fails"),
  Headers = #{basic_auth => {Token, Secret}},
  #{status_code := 404} =
    lsl_test_utils:api_call(get, "/matches/wrong-id", Headers),

  ForbiddenId =
    binary_to_list(lsl_matches:id(lsl:start_match(Rival, lsl_ai_dumb, 5))),

  #{status_code := 403} =
    lsl_test_utils:api_call(get, "/matches/" ++ ForbiddenId, Headers),

  {comment, ""}.

-spec get_match_ok(lsl_test_utils:config()) -> {comment, []}.
get_match_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  Headers = #{basic_auth => {Token, Secret}},
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),
  RivalName = lsl_players:name(Player2),
  DumbAIName = lsl_ai_dumb:name(),

  ct:comment("Matches are created"),
  P1MId = lsl_matches:id(lsl:start_match(PlayerId, lsl_ai_dumb, 3)),
  RivMId = lsl_matches:id(lsl:start_match(Rival, PlayerId, 3)),

  ct:comment("GET /matches/:mid returns all matches"),
  #{status_code := 200,
           body := Body1} =
    lsl_test_utils:api_call(get, "/matches/" ++ binary_to_list(P1MId), Headers),
  #{ <<"id">> := P1MId
   , <<"rival">> := #{<<"id">> := <<"lsl_ai_dumb">>, <<"name">> := DumbAIName}
   , <<"board">> := [ [true]
                    , [false, false]
                    , [false, false, false]
                    ]
   , <<"current-player">> := PlayerId
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body1),

  #{status_code := 200,
           body := Body2} =
    lsl_test_utils:api_call(
      get, "/matches/" ++ binary_to_list(RivMId), Headers),
  #{ <<"id">> := RivMId
   , <<"rival">> := #{<<"id">> := Rival, <<"name">> := RivalName}
   , <<"board">> := [ [false]
                    , [false, false]
                    , [false, false, false]
                    ]
   , <<"current-player">> := PlayerId
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body2),

  {comment, ""}.

-spec patch_match_wrong(lsl_test_utils:config()) -> {comment, []}.
patch_match_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),

  ct:comment("PATCH without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(patch, "/matches/id"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("PATCH with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(patch, "/matches/id", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("PATCH with usr/pwd fails"),
  Headers1 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(patch, "/matches/id", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("PATCH with wrong token/secret fails"),
  Headers2 = #{basic_auth => {"a bad token", "a bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(patch, "/matches/id", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("PATCH with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(patch, "/matches/id", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("PATCH without content-type fails"),
  MatchId =
    binary_to_list(lsl_matches:id(lsl:start_match(Rival, PlayerId, 3))),
  MatchUrl = "/matches/" ++ MatchId,

  Headers4 = #{basic_auth => {Token, Secret}},
  #{status_code := 415} = lsl_test_utils:api_call(patch, MatchUrl, Headers4),

  ct:comment("Something that's not json fails as well"),
  Headers5 = Headers4#{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = lsl_test_utils:api_call(patch, MatchUrl, Headers5),

  ct:comment("Even with the right type"),
  Headers = Headers4#{<<"content-type">> => <<"application/json">>},
  #{status_code := 400} = lsl_test_utils:api_call(patch, MatchUrl, Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, "{"),
  #{<<"error">> := <<"invalid json">>} = sr_json:decode(Body0),

  ct:comment("No row fails"),
  #{status_code := 400,
           body := Body00} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, "{}"),
  #{<<"error">> := <<"missing field: row">>} = sr_json:decode(Body00),

  ct:comment("No col fails"),
  ReqBody1 = sr_json:encode(#{row => 1}),
  #{status_code := 400,
           body := Body1} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, ReqBody1),
  #{<<"error">> := <<"missing field: col">>} = sr_json:decode(Body1),

  ct:comment("No length fails"),
  ReqBody2 = sr_json:encode(#{row => 1, col => 1}),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, ReqBody2),
  #{<<"error">> := <<"missing field: length">>} = sr_json:decode(Body2),

  ct:comment("Invalid row fails"),
  ReqBody3 = sr_json:encode(#{col => 1, row => false, length => 1}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, ReqBody3),
  #{<<"error">> := <<"invalid field: row">>} = sr_json:decode(Body3),

  ReqBody4 = sr_json:encode(#{col => 1, row => 20, length => 1}),
  #{status_code := 400,
           body := Body4} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, ReqBody4),
  #{<<"error">> := <<"out of bounds">>} = sr_json:decode(Body4),

  ct:comment("PATCH with wrong match fails"),
  #{status_code := 404} =
    lsl_test_utils:api_call(patch, "/matches/wrong-id", Headers),

  ct:comment("PATCH with not your match fails"),
  ForbiddenId =
    binary_to_list(lsl_matches:id(lsl:start_match(Rival, lsl_ai_dumb, 5))),

  #{status_code := 403} =
    lsl_test_utils:api_call(patch, "/matches/" ++ ForbiddenId, Headers),

  ct:comment("PATCH when not your turn fails"),
  NotYourTurnId =
    binary_to_list(lsl_matches:id(lsl:start_match(PlayerId, Rival, 3))),

  #{status_code := 403} =
    lsl_test_utils:api_call(patch, "/matches/" ++ NotYourTurnId, Headers),

  {comment, ""}.

-spec patch_match_ok(lsl_test_utils:config()) -> {comment, []}.
patch_match_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),
  RivalName = lsl_players:name(Player2),
  Headers =
    #{ basic_auth => {Token, Secret}
     , <<"content-type">> => <<"application/json">>
     },
  DumbAIName = lsl_ai_dumb:name(),

  MatchId = lsl_matches:id(lsl:start_match(Rival, PlayerId, 3)),
  MatchUrl = "/matches/" ++ binary_to_list(MatchId),

  AIMatchId = lsl_matches:id(lsl:start_match(PlayerId, lsl_ai_dumb, 3)),
  AIMatchUrl = "/matches/" ++ binary_to_list(AIMatchId),

  ct:comment("PATCHing a game actually crosses the sticks"),
  ReqBody1 = sr_json:encode(#{row => 3, col => 2, length => 2}),
  #{status_code := 200,
           body := Body1} =
    lsl_test_utils:api_call(patch, MatchUrl, Headers, ReqBody1),
  #{ <<"id">> := MatchId
   , <<"rival">> := #{<<"id">> := Rival, <<"name">> := RivalName}
   , <<"board">> := [ [false]
                    , [false, false]
                    , [false, true, true]
                    ]
   , <<"current-player">> := Rival
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body1),

  ct:comment("AI plays automatically"),
  ReqBody2 = sr_json:encode(#{row => 2, col => 1, length => 2}),
  #{status_code := 200,
           body := Body2} =
    lsl_test_utils:api_call(patch, AIMatchUrl, Headers, ReqBody2),
  #{ <<"id">> := AIMatchId
   , <<"rival">> := #{<<"id">> := <<"lsl_ai_dumb">>, <<"name">> := DumbAIName}
   , <<"board">> := [ [true]
                    , [true, true]
                    , [true, false, false]
                    ]
   , <<"current-player">> := PlayerId
   , <<"status">> := <<"playing">>
   } = sr_json:decode(Body2),

  {comment, ""}.

-spec delete_match_wrong(lsl_test_utils:config()) -> {comment, []}.
delete_match_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),

  ct:comment("DELETE without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(delete, "/matches/id"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("DELETE with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(delete, "/matches/id", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("DELETE with usr/pwd fails"),
  Headers1 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(delete, "/matches/id", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("DELETE with wrong token/secret fails"),
  Headers2 = #{basic_auth => {"a bad token", "a bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(delete, "/matches/id", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("DELETE with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(delete, "/matches/id", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("DELETE with wrong match fails"),
  Headers = #{basic_auth => {Token, Secret}},

  #{status_code := 404} =
    lsl_test_utils:api_call(delete, "/matches/wrong-id", Headers),

  ForbiddenId =
    binary_to_list(lsl_matches:id(lsl:start_match(Rival, lsl_ai_dumb, 5))),

  #{status_code := 403} =
    lsl_test_utils:api_call(delete, "/matches/" ++ ForbiddenId, Headers),

  {comment, ""}.

-spec delete_match_ok(lsl_test_utils:config()) -> {comment, []}.
delete_match_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  PlayerId = lsl_players:id(Player1),
  {session1, SessionA} = lists:keyfind(session1, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  Headers = #{basic_auth => {Token, Secret}},
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Rival = lsl_players:id(Player2),

  ct:comment("Matches are created"),
  P1MId = lsl_matches:id(lsl:start_match(PlayerId, lsl_ai_dumb, 3)),
  P1Url = "/matches/" ++ binary_to_list(P1MId),
  RivMId = lsl_matches:id(lsl:start_match(Rival, PlayerId, 3)),
  RivUrl = "/matches/" ++ binary_to_list(RivMId),

  ct:comment("DELETE /matches/:mid deletes the match"),
  true = lsl:is_match(P1MId),
  #{status_code := 204} = lsl_test_utils:api_call(delete, P1Url, Headers),
  ktn_task:wait_for(fun() -> lsl:is_match(P1MId) end, false),

  #{status_code := 404} = lsl_test_utils:api_call(delete, P1Url, Headers),

  ct:comment("and it works with other players as well"),
  true = lsl:is_match(RivMId),
  #{status_code := 204} = lsl_test_utils:api_call(delete, RivUrl, Headers),
  ktn_task:wait_for(fun() -> lsl:is_match(P1MId) end, false),

  #{status_code := 404} = lsl_test_utils:api_call(delete, RivUrl, Headers),

  {comment, ""}.
