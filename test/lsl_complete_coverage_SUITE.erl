-module(lsl_complete_coverage_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-dialyzer(no_opaque).
-dialyzer(no_return).

-export([all/0, web_utils/1, matches_repo/1, matches/1, players/1, sessions/1]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec web_utils(lsl_test_utils:config()) -> {comment, []}.
web_utils(_Config) ->
  {halt, _, state} = lsl_matches_handler:handle_post(req, state),
  {comment, ""}.

-spec matches_repo(lsl_test_utils:config()) -> {comment, []}.
matches_repo(_Config) ->
  PlayerId = lsl_players:id(lsl_test_utils:register_player(<<"player">>)),
  RivalId = lsl_players:id(lsl_test_utils:register_player(<<"rival">>)),
  MatchId = lsl_matches:id(lsl_test_utils:start_match(PlayerId, RivalId, 3)),
  try lsl_matches_repo:play(<<"wrong-match-id">>, PlayerId, 1, 1, 1) of
    X -> ct:fail("Unexpected result ~p", [X])
  catch
    _:notfound -> ok
  end,
  try lsl_matches_repo:play(MatchId, PlayerId, 1, 1, 1) of
    Y -> ct:fail("Unexpected result ~p", [Y])
  catch
    _:invalid_player -> ok
  end,
  {comment, ""}.

-spec matches(lsl_test_utils:config()) -> {comment, []}.
matches(_Config) ->
  Match = lsl_matches_repo:start(<<"p">>, lsl_ai_dumb, 5),

  ct:comment("from_json fails"),
  try lsl_matches:from_json(#{}) of
    R -> ct:fail("Unexpected result: ~p", [R])
  catch
    _:no_simple_parsing -> ok
  end,

  ct:comment("update fails"),
  try lsl_matches:update(Match, #{}) of
    R2 -> ct:fail("Unexpected result: ~p", [R2])
  catch
    _:no_simple_parsing -> ok
  end,

  {comment, ""}.

-spec players(lsl_test_utils:config()) -> {comment, []}.
players(_Config) ->
  Player = lsl_players:new(<<"n">>, <<"p">>),

  ct:comment("Update is ignored"),
  {ok, Player} = lsl_players:update(Player, #{}),

  {comment, ""}.

-spec sessions(lsl_test_utils:config()) -> {comment, []}.
sessions(_Config) ->
  Match = lsl_sessions:new(<<"p">>),

  ct:comment("from_json fails"),
  try lsl_sessions:from_json(#{}) of
    R -> ct:fail("Unexpected result: ~p", [R])
  catch
    _:no_simple_parsing -> ok
  end,

  ct:comment("update fails"),
  try lsl_sessions:update(Match, #{}) of
    R2 -> ct:fail("Unexpected result: ~p", [R2])
  catch
    _:no_simple_parsing -> ok
  end,

  {comment, ""}.
