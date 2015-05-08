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
             ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ post_sessions_wrong/1
        , post_sessions_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
init_per_testcase(TestCase, Config) ->
  Name = atom_to_binary(TestCase, utf8),
  try lsl:register_player(Name, <<"pwd">>) of
    Player ->
      [{player, Player} | Config]
  catch
    throw:conflict ->
      [Player | _] = sumo:find_by(lsl_players, [{name, Name}]),
      [{player, Player} | Config]
  end.

-spec end_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
end_per_testcase(_TestCase, Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  sumo:delete(lsl_players, lsl_players:id(Player)),
  lists:keydelete(player, 1, Config).

-spec post_sessions_wrong(lsl_test_utils:config()) -> {comment, []}.
post_sessions_wrong(Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  Name = binary_to_list(lsl_players:name(Player)),

  ct:comment("POST without auth fails"),
  #{status_code := 401,
        headers := #{<<"www-authenticate">> := <<"Basic realm=\"LSL\"">>}} =
    lsl_test_utils:api_call(post, "/sessions"),

  ct:comment("POST with wrong auth fails"),
  Headers0 = #{basic_auth => {"some name", "very bad pwd"}},
  #{status_code := 401,
        headers := #{<<"www-authenticate">> := <<"Basic realm=\"LSL\"">>}} =
    lsl_test_utils:api_call(post, "/sessions", Headers0),

  ct:comment("POST with wrong password fails"),
  Headers1 = #{basic_auth => {Name, "very bad pwd"}},
  #{status_code := 401,
        headers := #{<<"www-authenticate">> := <<"Basic realm=\"LSL\"">>}} =
    lsl_test_utils:api_call(post, "/sessions", Headers1),

  {comment, ""}.

-spec post_sessions_ok(lsl_test_utils:config()) -> {comment, []}.
post_sessions_ok(Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  Name = binary_to_list(lsl_players:name(Player)),
  Headers = #{basic_auth => {Name, "pwd"}},

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
