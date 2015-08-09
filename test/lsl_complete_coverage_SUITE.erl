-module(lsl_complete_coverage_SUITE).
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
             , web_utils/1
             , matches_repo/1
             ]).

-export([all/0, web_utils/1, matches_repo/1]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec web_utils(lsl_test_utils:config()) -> {comment, []}.
web_utils(_Config) ->
  {halt, _, state} = lsl_players_handler:handle_post(req, state),
  {comment, ""}.

-spec matches_repo(lsl_test_utils:config()) -> {comment, []}.
matches_repo(_Config) ->
  PlayerId = lsl_players:id(lsl_test_utils:register_player(<<"player">>)),
  RivalId = lsl_players:id(lsl_test_utils:register_player(<<"rival">>)),
  MatchId = lsl_matches:id(lsl:start_match(PlayerId, RivalId, 3)),
  try lsl:play(<<"wrong-match-id">>, PlayerId, 1, 1, 1) of
    X -> ct:fail("Unexpected result ~p", [X])
  catch
    _:notfound -> ok
  end,
  try lsl:play(MatchId, PlayerId, 1, 1, 1) of
    Y -> ct:fail("Unexpected result ~p", [Y])
  catch
    _:invalid_player -> ok
  end,
  {comment, ""}.
