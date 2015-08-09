%%% @doc AI behaviour definition.
-module(lsl_ai).
-author('elbrujohalcon@inaka.net').

-type move() :: {lsl_core:row(), lsl_core:col(), lsl_core:length()}.
-export_type([move/0]).

-callback next_move(lsl_core:match()) -> move().
-callback name() -> binary().

-export([play/2, fetch/1, is_ai/1, all/0, to_json/1]).

%% @doc plays a round
-spec play(module(), lsl_core:match()) ->
  {lsl_core:cross_result(), lsl_core:match()}.
play(Mod, Match) ->
  {Row, Col, Length} = Mod:next_move(Match),
  lsl_core:cross(Row, Col, Length, Match).

%% @doc Retrieves all AI players
-spec all() -> [module(),...].
all() -> application:get_env(lsl, ais, [lsl_ai_dumb]).

%% @doc Retrieves the module that provides a particular AI
-spec fetch(binary()) -> module() | notfound.
fetch(Id) ->
  try binary_to_existing_atom(Id, utf8) of
    Module ->
      case is_ai(Module) of
        true -> Module;
        false -> notfound
      end
  catch
    _:badarg -> notfound
  end.

%% @doc is this module AI?
-spec is_ai(module()) -> boolean().
is_ai(Module) -> lists:member(Module, all()).

%% @doc Represents the AI player as json
-spec to_json(module()) -> map().
to_json(Module) ->
  #{id => Module, name => Module:name()}.
