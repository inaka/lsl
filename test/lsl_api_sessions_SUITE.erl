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
-ignore_xref([ post_players_wrong/1
             , post_players_conflict/1
             , post_players_ok/1
             ]).

-export([all/0]).
-export([ post_sessions_auth/1
        , post_sessions_wrong/1
        , post_sessions_ok/1
        ]).

-spec all() -> [atom()].
all() -> lsl_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
init_per_testcase(TestCase, Config) ->
  Name = atom_to_binary(TestCase, utf8),
  try lsl_palyers_repo:register(Name, <<"pwd">>) of
    Player ->
      [{player, Player} | Config]
  catch
    throw:conflict ->
      [Player | _] = sumo:find_by(lsl_players, [{name, Name}]),
      [{player, Player} | Config]
  end.

-spec end_per_testcase(atom(), lsl_test_utils:config()) ->
        lsl_test_utils:config().
end_per_testcase(TestCase, Config) ->
  {player, Player} = lists:keyfind(player, 1, Config),
  sumo:delete(lsl_players, lsl_players:id(Player)),
  lists:keydelete(player, 1, Config).

-spec post_sessions_auth(lsl_test_utils:config()) -> {comment, []}.
post_sessions_auth(Config) ->
  %{player, Player} = lists:keyfind(player, 1, Config),
  %Name = lsl_players:name(Player),
  %Pwd = <<"pwd">>

  {comment, ""}.
