-module(lsl_api_sessions_SUITE).

-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-ignore_xref([all/0]).
-ignore_xref([ init_per_suite/1
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
init_per_testcase(post_sessions_wrong, Config) ->
  add_player(post_sessions_wrong, Config);
init_per_testcase(post_sessions_ok, Config) ->
  add_player(post_sessions_ok, Config);
init_per_testcase(delete_sessions_wrong, Config) ->
  add_sesssion2(
    add_player2(
      add_sesssions(
        add_player(delete_sessions_wrong, Config))));
init_per_testcase(delete_sessions_ok, Config) ->
  add_sesssion2(
    add_player2(
      add_sesssions(
        add_player(delete_sessions_ok, Config)))).

-spec end_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
end_per_testcase(delete_sessions_wrong, Config) ->
  {value, {player, Player}, Config1} = lists:keytake(player, 1, Config),
  {value, {player2, Player2}, Config2} = lists:keytake(player2, 1, Config1),
  sumo:delete(lsl_players, lsl_players:id(Player)),
  sumo:delete(lsl_players, lsl_players:id(Player2)),
  lists:keydelete(session2, 1, lists:keydelete(session, 1, Config2));
end_per_testcase(_TestCase, Config) ->
  {value, {player, Player}, Config1} = lists:keytake(player, 1, Config),
  sumo:delete(lsl_players, lsl_players:id(Player)),
  lists:keydelete(sessionb, 1, lists:keydelete(session, 1, Config1)).

-spec post_sessions_wrong(lsl_test_utils:config()) -> {comment, []}.
post_sessions_wrong(Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  Name = binary_to_list(lsl_players:name(Player)),

  ct:comment("POST without auth fails"),
  #{status_code := 401,
        headers := RHeaders0} = lsl_test_utils:api_call(post, "/sessions"),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("POST with wrong auth fails"),
  Headers0 = #{basic_auth => {"some name", "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(post, "/sessions", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("POST with wrong password fails"),
  Headers1 = #{basic_auth => {Name, "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(post, "/sessions", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"player\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  {comment, ""}.

-spec post_sessions_ok(lsl_test_utils:config()) -> {comment, []}.
post_sessions_ok(Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  Name = binary_to_list(lsl_players:name(Player)),
  Headers = #{ basic_auth => {Name, "pwd"}
             , <<"content-type">> => <<"application/json; charset=utf8">>
             },

  ct:comment("Create the session"),
  #{status_code := 201,
           body := RespBody} =
    lsl_test_utils:api_call(post, "/sessions", Headers),

  #{ <<"token">> := Token1
   , <<"secret">> := Secret1
   } = lsl_json:decode(RespBody),

  ct:comment("Create another session"),
  #{status_code := 201,
           body := RespBody2} =
    lsl_test_utils:api_call(post, "/sessions", Headers),

  #{ <<"token">> := Token2
   , <<"secret">> := Secret2
   } = lsl_json:decode(RespBody2),
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
  {player, Player} = lists:keyfind(player, 1, Config),
  Name = binary_to_list(lsl_players:name(Player)),
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),
  Secret = binary_to_list(lsl_sessions:secret(Session)),
  {session2, Session2} = lists:keyfind(session2, 1, Config),
  Token2 = binary_to_list(lsl_sessions:token(Session2)),

  ct:comment("DELETE without token fails"),
  #{status_code := 405} = lsl_test_utils:api_call(delete, "/sessions"),

  ct:comment("DELETE without auth fails"),
  #{status_code := 401,
        headers := RHeaders0} = lsl_test_utils:api_call(delete, "/sessions/0"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("DELETE with wrong auth fails"),
  Headers0 = #{basic_auth => {"some name", "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(delete, "/sessions/1", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("DELETE with wrong password fails"),
  Headers1 = #{basic_auth => {Name, "very bad pwd"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(delete, "/sessions/2", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  ct:comment("DELETE with wrong secret fails"),
  Headers2 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders3} =
    lsl_test_utils:api_call(delete, "/sessions/3", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders3),

  ct:comment("DELETE session from other player fails"),
  Headers3 = #{basic_auth => {Name, "pwd"}},
  #{status_code := 403} =
    lsl_test_utils:api_call(delete, "/sessions/" ++ Token2, Headers3),

  ct:comment("DELETE session from other player (using token) fails"),
  Headers4 = #{basic_auth => {Token, Secret}},
  #{status_code := 403} =
    lsl_test_utils:api_call(delete, "/sessions/" ++ Token2, Headers4),

  {comment, ""}.

-spec delete_sessions_ok(lsl_test_utils:config()) -> {comment, []}.
delete_sessions_ok(Config) ->
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),
  Secret = binary_to_list(lsl_sessions:secret(Session)),
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
  #{status_code := 204} = lsl_test_utils:api_call(delete, Url2, Headers2),

  ct:comment("DELETE other session with token/secret"),
  #{status_code := 204} = lsl_test_utils:api_call(delete, UrlB, Headers1),
  #{status_code := 204} = lsl_test_utils:api_call(delete, UrlB, Headers1),

  ct:comment("DELETE same session with token/secret"),
  #{status_code := 204} = lsl_test_utils:api_call(delete, Url1, Headers1),

  {comment, ""}.



add_player(TestCase, Config) ->
  Name = atom_to_binary(TestCase, utf8),
  try lsl:register_player(Name, <<"pwd">>) of
    Player ->
      [{player, Player} | Config]
  catch
    throw:conflict ->
      [Player | _] = sumo:find_by(lsl_players, [{name, Name}]),
      [{player, Player} | Config]
  end.

add_sesssions(Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  PlayerId = lsl_players:id(Player),
  [ {session, lsl:open_session(PlayerId)}
  , {sessionb, lsl:open_session(PlayerId)}
  | Config].

add_player2(Config) ->
  Name = <<"player2">>,
  try lsl:register_player(Name, <<"pwd">>) of
    Player ->
      [{player2, Player} | Config]
  catch
    throw:conflict ->
      [Player | _] = sumo:find_by(lsl_players, [{name, Name}]),
      [{player2, Player} | Config]
  end.

add_sesssion2(Config) ->
  {player2, Player} = lists:keyfind(player2, 1, Config),
  PlayerId = lsl_players:id(Player),
  [{session2, lsl:open_session(PlayerId)} | Config].
