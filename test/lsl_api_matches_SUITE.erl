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
        headers := RHeaders0} = lsl_test_utils:api_call(post, "/matches"),
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
  #{<<"error">> := <<"invalid json">>} = lsl_json:decode(Body0),

  ct:comment("No rival fails"),
  #{status_code := 400,
           body := Body1} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{}"),
  #{<<"error">> := <<"missing field: rival">>} = lsl_json:decode(Body1),

  ct:comment("Invalid rival fails"),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{\"rival\":\"false\"}"),
  #{<<"error">> := <<"invalid field: rival">>} = lsl_json:decode(Body2),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(post, "/matches", Headers, "{\"rival\":\"io\"}"),

  ct:comment("Invalid rows fails"),
  ReqBody3 = lsl_json:encode(#{rival => lsl_ai_dumb, rows => -1}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody3),
  #{<<"error">> := <<"invalid field: rows">>} = lsl_json:decode(Body3),

  ReqBody4 = lsl_json:encode(#{rival => Rival, rows => 0}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody4),

  ReqBody5 = lsl_json:encode(#{rival => lsl_ai_dumb, rows => false}),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/matches", Headers, ReqBody5),

  ReqBody6 = lsl_json:encode(#{rival => Rival, rows => 1}),
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
  ReqBody1 = lsl_json:encode(#{rival => Rival}),
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
   } = lsl_json:decode(Body1),

  ct:comment("Start a match against another player, 2 rows"),
  ReqBody2 = lsl_json:encode(#{rival => Rival, rows => 2}),
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
   } = lsl_json:decode(Body2),
  case Id2 of
    Id1 -> ct:fail("Duplicated game");
    Id2 -> ok
  end,

  ct:comment("Start a match against AI, default # of rows"),
  ReqBody3 = lsl_json:encode(#{rival => lsl_ai_dumb}),
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
   } = lsl_json:decode(Body3),

  ct:comment("Start a match against AI player, 2 rows"),
  ReqBody4 = lsl_json:encode(#{rival => lsl_ai_rodolfo}),
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
   } = lsl_json:decode(Body4),
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
        headers := RHeaders0} = lsl_test_utils:api_call(get, "/matches"),
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
  [] = lsl_json:decode(EmptyBody),

  ct:comment("A match that doesn't include player1 is created"),
  lsl:start_match(Rival, lsl_ai_dumb, 5),

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
  ] = lsl_json:decode(Body1),

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
  ] = lsl_json:decode(Body2) -- [Match1],

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

  ct:comment("Three matches are created"),
  PlayingId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  WonId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  LostId = lsl_matches:id(lsl:start_match(PlayerId, Rival, 3)),
  AllIds = lists:sort([PlayingId, WonId, LostId]),

  ct:comment("Player1 wins one, looses 1"),
  lsl:play(WonId, Rival, 3, 1, 3),
  lsl:play(WonId, PlayerId, 2, 1, 2),
  lsl:play(LostId, Rival, 3, 1, 3),
  lsl:play(LostId, PlayerId, 1, 1, 1),
  lsl:play(LostId, Rival, 2, 1, 1),

  MatchIds =
    fun(Status) ->
      #{status_code := 200,
               body := Body} =
        lsl_test_utils:api_call(get, "/matches?status=" ++ Status, Headers),
      lists:sort([MatchId || #{<<"id">> := MatchId} <- lsl_json:decode(Body)])
    end,

  ct:comment("GET /matches?status=all returns all of them"),
  AllIds = MatchIds("all"),

  ct:comment("GET /matches?status=won returns the on that the player won"),
  [WonId] = MatchIds("won"),

  ct:comment("GET /matches?status=lost returns the on that the player lost"),
  [LostId] = MatchIds("lost"),

  ct:comment("GET /matches?status=playing returns the on that's still ongoing"),
  [PlayingId] = MatchIds("playing"),

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
        headers := RHeaders0} = lsl_test_utils:api_call(get, "/matches/id"),
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

  ForbiddenId = lsl_matches:id(lsl:start_match(Rival, lsl_ai_dumb, 5)),

  #{status_code := 403} =
    lsl_test_utils:api_call(get, <<"/matches/", ForbiddenId/binary>>, Headers),

  {comment, ""}.
