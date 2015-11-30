-module(lsl_api_sessions_SUITE).
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
-ignore_xref([ post_sessions_wrong/1
             , post_sessions_ok/1
             , delete_sessions_wrong/1
             , delete_sessions_ok/1
             ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ post_sessions_wrong/1
        , post_sessions_ok/1
        , delete_sessions_wrong/1
        , delete_sessions_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
init_per_testcase(TestCase, Config) ->
  Name = atom_to_binary(TestCase, utf8),
  Player1 = lsl_test_utils:register_player(Name),
  SessionA = lsl_test_utils:open_session(Player1),
  SessionB = lsl_test_utils:open_session(Player1),
  Player2 = lsl_test_utils:register_player(<<Name/binary, "-2">>),
  Session2 = lsl_test_utils:open_session(Player2),
  [ {player1, Player1}
  , {sessiona, SessionA}
  , {sessionb, SessionB}
  , {player2, Player2}
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
  {value, {sessiona, SessionA}, Config3} = lists:keytake(sessiona, 1, Config2),
  lsl:close_session(lsl_sessions:token(SessionA)),
  {value, {sessionb, SessionB}, Config4} = lists:keytake(sessionb, 1, Config3),
  lsl:close_session(lsl_sessions:token(SessionB)),
  {value, {session2, Session2}, Config5} = lists:keytake(session2, 1, Config4),
  lsl:close_session(lsl_sessions:token(Session2)),
  Config5.

-spec post_sessions_wrong(lsl_test_utils:config()) -> {comment, []}.
post_sessions_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),

  ct:comment("POST without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(post, "/sessions"),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("POST with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(post, "/sessions", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("POST with wrong auth fails"),
  Headers1 = #{basic_auth => {"some name", "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(post, "/sessions", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("POST with wrong password fails"),
  Headers2 = #{basic_auth => {Name, "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(post, "/sessions", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  {comment, ""}.

-spec post_sessions_ok(lsl_test_utils:config()) -> {comment, []}.
post_sessions_ok(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  Headers = #{ basic_auth => {Name, "pwd"}
             , <<"content-type">> => <<"application/json; charset=utf8">>
             },

  ct:comment("Create the session"),
  #{status_code := 201,
           body := RespBody} =
    lsl_test_utils:api_call(post, "/sessions", Headers),

  #{ <<"token">> := Token1
   , <<"secret">> := Secret1
   } = sr_json:decode(RespBody),

  ct:comment("Create another session"),
  #{status_code := 201,
           body := RespBody2} =
    lsl_test_utils:api_call(post, "/sessions", Headers),

  #{ <<"token">> := Token2
   , <<"secret">> := Secret2
   } = sr_json:decode(RespBody2),
  case Token2 of
    Token1 -> ct:fail("Duplicated Token: ~p", [Token2]);
    Token2 -> ok
  end,
  case Secret2 of
    Secret1 -> ct:fail("Duplicated secret: ~p", [Secret2]);
    Secret2 -> ok
  end,

  {comment, ""}.

-spec delete_sessions_wrong(lsl_test_utils:config()) -> {comment, []}.
delete_sessions_wrong(Config) ->
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  Name = binary_to_list(lsl_players:name(Player1)),
  {sessiona, SessionA} = lists:keyfind(sessiona, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {session2, Session2} = lists:keyfind(session2, 1, Config),
  Token2 = binary_to_list(lsl_sessions:token(Session2)),

  ct:comment("DELETE without token fails"),
  #{status_code := 405} = lsl_test_utils:api_call(delete, "/sessions"),

  ct:comment("DELETE without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(delete, "/sessions/0"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("DELETE with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(delete, "/sessions/0", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("DELETE with wrong auth fails"),
  Headers1 = #{basic_auth => {"some name", "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(delete, "/sessions/1", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("DELETE with wrong password fails"),
  Headers2 = #{basic_auth => {Name, "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(delete, "/sessions/2", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("DELETE with wrong secret fails"),
  Headers3 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(delete, "/sessions/3", Headers3),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("DELETE session from other player fails"),
  Headers4 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 403} =
    lsl_test_utils:api_call(delete, "/sessions/" ++ Token2, Headers4),

  ct:comment("DELETE session from other player (using token) fails"),
  Headers5 = #{basic_auth => {Token, Secret}},
  #{status_code := 403} =
    lsl_test_utils:api_call(delete, "/sessions/" ++ Token2, Headers5),

  {comment, ""}.

-spec delete_sessions_ok(lsl_test_utils:config()) -> {comment, []}.
delete_sessions_ok(Config) ->
  {sessiona, SessionA} = lists:keyfind(sessiona, 1, Config),
  Token = binary_to_list(lsl_sessions:token(SessionA)),
  Secret = binary_to_list(lsl_sessions:secret(SessionA)),
  {sessionb, SessionB} = lists:keyfind(sessionb, 1, Config),
  TokenB = binary_to_list(lsl_sessions:token(SessionB)),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  Name2 = binary_to_list(lsl_players:name(Player2)),
  {session2, Session2} = lists:keyfind(session2, 1, Config),
  Token2 = binary_to_list(lsl_sessions:token(Session2)),

  Headers1 = #{basic_auth => {Token, Secret}},
  Headers2 = #{basic_auth => {Name2, "pwd"}},
  Url1 = "/sessions/" ++ Token,
  UrlB = "/sessions/" ++ TokenB,
  Url2 = "/sessions/" ++ Token2,

  ct:comment("DELETE with name/pwd"),
  #{status_code := 204} = lsl_test_utils:api_call(delete, Url2, Headers2),
  #{status_code := 404} = lsl_test_utils:api_call(delete, Url2, Headers2),

  ct:comment("DELETE other session with token/secret"),
  #{status_code := 204} = lsl_test_utils:api_call(delete, UrlB, Headers1),
  #{status_code := 404} = lsl_test_utils:api_call(delete, UrlB, Headers1),

  ct:comment("DELETE same session with token/secret"),
  #{status_code := 204} = lsl_test_utils:api_call(delete, Url1, Headers1),

  {comment, ""}.
