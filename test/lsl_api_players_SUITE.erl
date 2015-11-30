-module(lsl_api_players_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ post_players_wrong/1
        , post_players_conflict/1
        , post_players_ok/1
        , get_players_wrong/1
        , get_players_ok/1
        , get_ai_players_wrong/1
        , get_ai_players_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
init_per_testcase(TestCase, Config) ->
  Name = atom_to_binary(TestCase, utf8),
  Player = lsl_test_utils:register_player(Name),
  Session = lsl_test_utils:open_session(Player),
  [{player, Player}, {session, Session} | Config].

-spec end_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
end_per_testcase(_TestCase, Config) ->
  {value, {player, Player}, Config1} = lists:keytake(player, 1, Config),
  sumo:delete(lsl_players, lsl_players:id(Player)),
  {value, {session, Session}, Config2} = lists:keytake(session, 1, Config1),
  lsl:close_session(lsl_sessions:token(Session)),
  Config2.

-spec post_players_wrong(lsl_test_utils:config()) -> {comment, []}.
post_players_wrong(_Config) ->
  ct:comment("POST with empty body fails"),
  #{status_code := 415} = lsl_test_utils:api_call(post, "/players"),

  ct:comment("Something that's not json fails as well"),
  Headers2 = #{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = lsl_test_utils:api_call(post, "/players", Headers2),

  ct:comment("Even with correct type"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  #{status_code := 400} = lsl_test_utils:api_call(post, "/players", Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400} =
    lsl_test_utils:api_call(post, "/players", Headers, "{"),

  ct:comment("No name fails"),
  #{status_code := 400} =
    lsl_test_utils:api_call(post, "/players", Headers, "{}"),

  ct:comment("No password fails"),
  #{status_code := 400} =
    lsl_test_utils:api_call(post, "/players", Headers, "{\"name\":\"joe\"}"),

  {comment, ""}.

-spec post_players_conflict(lsl_test_utils:config()) -> {comment, []}.
post_players_conflict(_Config) ->
  ct:comment("First create a user"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Body =
    sr_json:encode(
      #{name => <<"test-conflict-player">>, password => <<"ap455w0rd">>}),
  #{status_code := 201} =
    lsl_test_utils:api_call(post, "/players", Headers, Body),

  ct:comment("Try to create same user again"),
  #{status_code := 409} =
    lsl_test_utils:api_call(post, "/players", Headers, Body),

  ct:comment("Even with different password"),
  Body2 =
    sr_json:encode(
      #{name => <<"test-conflict-player">>, password => <<"another-1">>}),
  #{status_code := 409} =
    lsl_test_utils:api_call(post, "/players", Headers, Body2),

  {comment, ""}.

-spec post_players_ok(lsl_test_utils:config()) -> {comment, []}.
post_players_ok(_Config) ->
  ct:comment("Make sure not to have the players already"),
  sumo:delete_by(lsl_players, [{name, <<"test-ok-player">>}]),
  sumo:delete_by(lsl_players, [{name, <<"test-ok-player-2">>}]),

  ct:comment("Create the player"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Body =
    sr_json:encode(
      #{name => <<"test-ok-player">>, password => <<"ap455w0rd">>}),
  #{status_code := 201,
           body := RespBody} =
    lsl_test_utils:api_call(post, "/players", Headers, Body),

  RespMap =
    #{ <<"id">> := Id1
     , <<"name">> := <<"test-ok-player">>
     } = sr_json:decode(RespBody),
  false = maps:is_key(<<"password">>, RespMap),

  ct:comment("Create another player"),
  Body2 =
    sr_json:encode(
      #{name => <<"test-ok-player-2">>, password => <<"ap455w0rd">>}),
  #{status_code := 201,
           body := RespBody2} =
    lsl_test_utils:api_call(post, "/players", Headers, Body2),

  #{ <<"id">> := Id2
   , <<"name">> := <<"test-ok-player-2">>
   } = sr_json:decode(RespBody2),
  case Id2 of
    Id1 -> ct:fail("Duplicated id: ~p", [Id2]);
    Id2 -> ok
  end,

  {comment, ""}.

-spec get_players_wrong(lsl_test_utils:config()) -> {comment, []}.
get_players_wrong(Config) ->
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),

  ct:comment("GET without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(get, "/players"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("GET with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(get, "/players", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("GET with wrong auth fails"),
  Headers1 = #{basic_auth => {"some token", "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(get, "/players", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("GET with wrong secret fails"),
  Headers2 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(get, "/players", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  {comment, ""}.

-spec get_players_ok(lsl_test_utils:config()) -> {comment, []}.
get_players_ok(Config) ->
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),
  Secret = binary_to_list(lsl_sessions:secret(Session)),
  Headers = #{basic_auth => {Token, Secret}},

  ct:comment("GET /players returns at least the caller"),
  #{status_code := 200,
           body := Body1} =
    lsl_test_utils:api_call(get, "/players", Headers),
  Players1 = sr_json:decode(Body1),
  [_] =
    [Id || #{<<"id">> := Id, <<"name">> := <<"get_players_ok">>} <- Players1],
  [] = [Pwd || #{<<"password">> := Pwd} <- Players1],

  ct:comment("When a player is added, GET /players should return it"),
  _ = lsl_test_utils:register_player(<<"get_players_ok-2">>, <<"pwd">>),
  #{status_code := 200,
           body := Body2} =
    lsl_test_utils:api_call(get, "/players", Headers),
  Players2 = sr_json:decode(Body2),
  [#{<<"name">> := <<"get_players_ok-2">>}] = Players2 -- Players1,

  {comment, ""}.

-spec get_ai_players_wrong(lsl_test_utils:config()) -> {comment, []}.
get_ai_players_wrong(Config) ->
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),

  ct:comment("GET without auth fails"),
  #{status_code := 401,
        headers := RHeadersZ} = lsl_test_utils:api_call(get, "/ai-players"),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeadersZ),

  ct:comment("GET with any auth fails"),
  Headers0 = #{<<"authorization">> => <<"something wrong">>},
  #{status_code := 401,
        headers := RHeaders0} =
    lsl_test_utils:api_call(get, "/ai-players", Headers0),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders0),

  ct:comment("GET with wrong auth fails"),
  Headers1 = #{basic_auth => {"some token", "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders1} =
    lsl_test_utils:api_call(get, "/ai-players", Headers1),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders1),

  ct:comment("GET with wrong secret fails"),
  Headers2 = #{basic_auth => {Token, "very bad secret"}},
  #{status_code := 401,
        headers := RHeaders2} =
    lsl_test_utils:api_call(get, "/ai-players", Headers2),
  {<<"www-authenticate">>, <<"Basic realm=\"session\"">>} =
    lists:keyfind(<<"www-authenticate">>, 1, RHeaders2),

  {comment, ""}.

-spec get_ai_players_ok(lsl_test_utils:config()) -> {comment, []}.
get_ai_players_ok(Config) ->
  {session, Session} = lists:keyfind(session, 1, Config),
  Token = binary_to_list(lsl_sessions:token(Session)),
  Secret = binary_to_list(lsl_sessions:secret(Session)),
  Headers = #{basic_auth => {Token, Secret}},
  DumbAIName = lsl_ai_dumb:name(),

  ct:comment("GET /ai-players returns at least the dumb"),
  #{status_code := 200,
           body := Body1} =
    lsl_test_utils:api_call(get, "/ai-players", Headers),
  Players1 = sr_json:decode(Body1),
  [_] =
    [Name || #{<<"id">> := <<"lsl_ai_dumb">>, <<"name">> := Name} <- Players1
         , Name == DumbAIName
         ],

  {comment, ""}.
