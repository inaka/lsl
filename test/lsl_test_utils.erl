%% @doc General Test Utils
-module(lsl_test_utils).

-type config() :: proplists:proplist().
-export_type([config/0]).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/3
        , api_call/4
        ]).
-export([ register_player/1
        , register_player/2
        , open_session/1
        ]).
-export([full_match/2]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = lsl:start(),
  {ok, _} = shotgun:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  _ = lsl:stop(),
  _ = shotgun:stop(),
  Config.

-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(atom(), string(), #{}) -> #{}.
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call(atom(), string(), #{}, iodata()) -> #{}.
api_call(Method, Uri, Headers, Body) ->
  Port = application:get_env(lsl, http_port, 8585),
  {ok, Pid} = shotgun:open("localhost", Port),
  try
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.

-spec full_match(module(), lsl_core:match()) -> ok.
full_match(Mod, Match) ->
  do_full_match(Mod, lsl_ai:play(Mod, Match)).

do_full_match(_Mod, {lost, _Match}) -> ok;
do_full_match(Mod, {_, Match}) -> do_full_match(Mod, lsl_ai:play(Mod, Match)).

-spec register_player(binary()) -> lsl_players:player().
register_player(Name) -> register_player(Name, <<"pwd">>).

-spec register_player(binary(), binary()) -> lsl_players:player().
register_player(Name, Password) ->
  sumo:persist(lsl_players, lsl_players:new(Name, Password)).

-spec open_session(lsl_players:player()) -> lsl_sessions:session().
open_session(Player) ->
  sumo:persist(lsl_sessions, lsl_sessions:new(lsl_players:id(Player))).
