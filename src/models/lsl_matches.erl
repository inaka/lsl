%%% @doc Matches document
-module(lsl_matches).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque match() ::
  #{ id => binary()
   , player_id => binary()
   , rival_kind => player | ai
   , rival => atom() | binary()
   , core => lsl_core:match()
   , turn => pos_integer()
   , created_at => dcn_datetime:datetime()
   , updated_at => dcn_datetime:datetime()
   }.
-export_type([match/0]).

-export([new/4, to_json/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([id/1, core/1, core/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,              binary,   [id, not_null])
    , sumo:new_field(player_id,       binary,   [not_null])
    , sumo:new_field(rival_kind,      binary,   [not_null])
    , sumo:new_field(rival,           binary,   [not_null])
    , sumo:new_field(core,            binary,   [not_null])
    , sumo:new_field(turn,            integer,  [not_null])
    , sumo:new_field(created_at,      datetime, [not_null])
    , sumo:new_field(updated_at,      datetime, [not_null])
    ]).

%% @private
-spec sumo_sleep(match()) -> sumo:doc().
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
-spec sumo_wakeup(sumo:doc()) -> match().
sumo_wakeup(Doc) ->
  #{ rival_kind := RivalKind
   , rival := Rival
   , core := Core
   } = Doc,
  Doc#{ rival_kind := binary_to_atom(RivalKind, utf8)
      , rival := case RivalKind of
                   <<"ai">> -> lsl:fetch_ai(Rival);
                   <<"player">> -> Rival
                 end
      , core := binary_to_term(Core)
      }.

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
   , turn       => 1
   , created_at => Now
   , updated_at => Now
   }.

%% @doc Represents a match as json
-spec to_json(match(), binary()) -> map().
to_json(Match, CallerId) ->
  #{ id         := Id
   , player_id  := PlayerId
   , rival_kind := RivalKind
   , rival      := Rival
   , core       := Core
   , turn       := Turn
   , created_at := CreatedAt
   , updated_at := UpdatedAt
   } = Match,
  CurrentPlayer =
    case Turn rem 2 of
      0 -> PlayerId;
      1 -> Rival
    end,
  #{ id => Id
   , rival =>
      case {CallerId, RivalKind} of
        {PlayerId, ai} -> lsl_ai:to_json(Rival);
        {PlayerId, player} -> lsl_players:to_json(lsl:fetch_player(Rival));
        {Rival, player} -> lsl_players:to_json(lsl:fetch_player(PlayerId))
      end
   , board => lsl_core:to_json(Core)
   , 'current-player' => CurrentPlayer
   , status =>
      match_status(CallerId, CurrentPlayer, lsl_core:last_result(Core))
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

%% @doc updates the match core, updated_at and turn #
-spec core(match(), lsl_core:match()) -> match().
core(Match, Core) ->
  #{turn := Turn} = Match,
  Match#{ turn := Turn + 1
        , core := Core
        , updated_at := ktn_date:now_human_readable()
        }.

match_status(_CallerId, _CurrentPlayer, next) -> <<"playing">>;
match_status(CallerId, CallerId, won) -> <<"lost">>;
match_status(CallerId, _Rival, won) -> <<"won">>;
match_status(CallerId, CallerId, lost) -> <<"won">>;
match_status(CallerId, _Rival, lost) -> <<"lost">>.
