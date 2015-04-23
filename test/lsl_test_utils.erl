%% @doc General Test Utils
-module(lsl_test_utils).

-type config() :: proplists:proplist().
-export_type([config/0]).

-export([all/1]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].
