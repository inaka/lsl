%%% @doc Matches document
-module(lsl_matches).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-opaque match() ::
  #{ id => binary() | undefined
   , player_id => binary()
   , rival_kind => player | ai
   , rival => atom() | binary()
   , core => lsl_core:match()
   , created_at => binary()
   , updated_at => binary()
   }.
-export_type([match/0]).

-type status() :: won | lost | playing.
-export_type([status/0]).

-export([new/4, to_json/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([to_json/1, from_json/1, update/2, uri_path/1]).
-export([ id/1
        , core/1
        , core/2
        , current_player/1
        , status/2
        , players/1
        , rival_kind/1
        , rival/1
        , location/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,              binary, [id, not_null])
    , sumo:new_field(player_id,       binary, [not_null])
    , sumo:new_field(rival_kind,      binary, [not_null])
    , sumo:new_field(rival,           binary, [not_null])
    , sumo:new_field(core,            binary, [not_null])
    , sumo:new_field(created_at,      binary, [not_null])
    , sumo:new_field(updated_at,      binary, [not_null])
    ]).

%% @private
-spec sumo_sleep(match()) -> sumo:model().
sumo_sleep(Match) ->
  #{ rival_kind := RivalKind
   , rival := Rival
   , core := Core
   } = Match,
  Match#{ rival_kind := atom_to_binary(RivalKind, utf8)
        , rival := case RivalKind of
                     ai -> atom_to_binary(Rival, utf8);
                     player -> Rival
                   end
        , core := term_to_binary(Core)
        }.

%% @private
-spec sumo_wakeup(sumo:model()) -> match().
sumo_wakeup(Doc) ->
  #{ rival_kind := RivalKind
   , rival := Rival
   , core := Core
   } = Doc,
  Doc#{ rival_kind := binary_to_atom(RivalKind, utf8)
      , rival := case RivalKind of
                   <<"ai">> -> lsl_ai:fetch(Rival);
                   <<"player">> -> Rival
                 end
      , core := binary_to_term(Core)
      }.

-spec to_json(match()) -> sr_json:json().
to_json(Match) -> to_json(Match, current_player(Match)).

-spec from_json(sumo_rest_doc:json()) -> no_return().
from_json(_Json) -> throw(no_simple_parsing).

-spec update(match(), sumo_rest_doc:json()) -> no_return().
update(_Match, _Changes) -> throw(no_simple_parsing).

-spec uri_path(match()) -> iodata().
uri_path(#{id := Id}) -> <<$/, Id/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a match
-spec new(binary(), player, binary(), lsl_core:match()) -> match();
         (binary(), ai, module(), lsl_core:match()) -> match().
new(PlayerId, RivalKind, Rival, Core) ->
  Now = ktn_date:now_human_readable(),
  #{ id         => undefined
   , player_id  => PlayerId
   , rival_kind => RivalKind
   , rival      => Rival
   , core       => Core
   , created_at => Now
   , updated_at => Now
   }.

%% @doc Represents a match as json
-spec to_json(match(), binary()) -> sr_json:json().
to_json(Match, CallerId) ->
  #{ id         := Id
   , player_id  := PlayerId
   , rival_kind := RivalKind
   , rival      := Rival
   , core       := Core
   , created_at := CreatedAt
   , updated_at := UpdatedAt
   } = Match,
  CurrentPlayer = current_player(Match),
  #{ id => Id
   , rival =>
      case {CallerId, RivalKind} of
        {PlayerId,  ai} -> Rival;
        {PlayerId,  player} -> Rival;
        {Rival,     player} -> PlayerId
      end
   , board => lsl_core:to_json(Core)
   , 'current-player' => CurrentPlayer
   , status => atom_to_binary(status(Match, CallerId), utf8)
   , created_at => CreatedAt
   , updated_at => UpdatedAt
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc id
-spec id(match()) -> binary().
id(#{id := Id}) -> Id.

%% @doc core
-spec core(match()) -> lsl_core:match().
core(#{core := Core}) -> Core.

%% @doc updates the match core, updated_at
-spec core(match(), lsl_core:match()) -> match().
core(Match, Core) ->
  Match#{ core := Core
        , updated_at := ktn_date:now_human_readable()
        }.

-spec current_player(match()) -> binary().
current_player(#{core := Core, rival := Rival, player_id := PlayerId}) ->
  case lsl_core:turns(Core) of
    Turns when Turns rem 2 == 0 -> Rival;
    _ -> PlayerId
  end.

-spec status(match(), binary()) -> status().
status(Match, CallerId) ->
  CurrentPlayer = current_player(Match),
  Core = core(Match),
  status(CallerId, CurrentPlayer, lsl_core:last_result(Core)).

status(_CallerId, _CurrentPlayer, next) -> playing;
status(CallerId,  CallerId,       won) ->  lost;
status(_CallerId, _Rival,         won) ->  won;
status(CallerId,  CallerId,       lost) -> won;
status(_CallerId, _Rival,         lost) -> lost.

%% @doc match players
-spec players(match()) -> [binary(), ...].
players(#{player_id := PlayerId, rival := Rival}) -> [PlayerId, Rival].

%% @doc is it a match against AI or another player?
-spec rival_kind(match()) -> player | ai.
rival_kind(#{rival_kind := RivalKind}) -> RivalKind.

%% @doc is it a match against AI or another player?
-spec rival(match()) -> atom() | binary().
rival(#{rival := Rival}) -> Rival.

-spec location(match(), sumo_rest_doc:path()) -> binary().
location(#{id := Id}, Path) -> iolist_to_binary([Path, "/", Id]).