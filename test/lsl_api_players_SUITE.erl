-module(lsl_api_players_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-ignore_xref([all/0]).
-ignore_xref([init_per_suite/1, end_per_suite/1]).
-ignore_xref([ post_players_wrong/1
             , post_players_conflict/1
             , post_players_ok/1
             ]).

-export([all/0]).
-export([ post_players_wrong/1
        , post_players_conflict/1
        , post_players_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

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
  #{status_code := 400,
           body := Body0} =
    lsl_test_utils:api_call(post, "/players", Headers, "{"),
  #{<<"error">> := <<"invalid json">>} = lsl_json:decode(Body0),

  ct:comment("No name fails"),
  #{status_code := 400,
           body := Body2} =
    lsl_test_utils:api_call(post, "/players", Headers, "{}"),
  #{<<"error">> := <<"missing field: name">>} = lsl_json:decode(Body2),

  ct:comment("No password fails"),
  #{status_code := 400,
           body := Body3} =
    lsl_test_utils:api_call(post, "/players", Headers, "{\"name\":\"joe\"}"),
  #{<<"error">> := <<"missing field: password">>} = lsl_json:decode(Body3),

  {comment, ""}.

-spec post_players_conflict(lsl_test_utils:config()) -> {comment, []}.
post_players_conflict(_Config) ->
  ct:comment("First create a user"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Body =
    lsl_json:encode(
      #{name => <<"test-conflict-player">>, password => <<"ap455w0rd">>}),
  lsl_test_utils:api_call(post, "/players", Headers, Body),

  ct:comment("Try to create same user again"),
  #{status_code := 409} =
    lsl_test_utils:api_call(post, "/players", Headers, Body),

  ct:comment("Even with different password"),
  Body2 =
    lsl_json:encode(
      #{name => <<"test-conflict-player">>, password => <<"another-1">>}),
  #{status_code := 409} =
    lsl_test_utils:api_call(post, "/players", Headers, Body2),

  {comment, ""}.

-spec post_players_ok(lsl_test_utils:config()) -> {comment, []}.
post_players_ok(_Config) ->
  ct:comment("Make sure to delete the players"),
  sumo:delete_by(lsl_players, [{name, <<"test-ok-player">>}]),
  sumo:delete_by(lsl_players, [{name, <<"test-ok-player-2">>}]),

  ct:comment("Create the player"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Body =
    lsl_json:encode(
      #{name => <<"test-ok-player">>, password => <<"ap455w0rd">>}),
  #{status_code := 201,
           body := RespBody} =
    lsl_test_utils:api_call(post, "/players", Headers, Body),

  RespMap =
    #{ <<"id">> := Id1
     , <<"name">> := <<"test-ok-player">>} = lsl_json:decode(RespBody),
  false = maps:is_key(<<"password">>, RespMap),

  ct:comment("Create another player"),
  Body2 =
    lsl_json:encode(
      #{name => <<"test-ok-player-2">>, password => <<"ap455w0rd">>}),
  #{status_code := 201,
           body := RespBody2} =
    lsl_test_utils:api_call(post, "/players", Headers, Body2),

  #{ <<"id">> := Id2
   , <<"name">> := <<"test-ok-player-2">>} = lsl_json:decode(RespBody2),
  case Id2 of
    Id1 -> ct:fail("Duplicated id: ~p", [Id2]);
    Id2 -> ok
  end,

  {comment, ""}.
