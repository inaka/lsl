%%% @doc Players document
-module(lsl_players).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque player() ::
  #{
    id => integer(),
    name => binary(),
    password => binary(),
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([player/0]).

-export([new/2, to_json/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          integer,  [id, not_null])
    , sumo:new_field(name,        string,   [not_null, index])
    , sumo:new_field(password,    string,   [not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    , sumo:new_field(updated_at,  datetime, [not_null])
    ]).

%% @private
-spec sumo_sleep(player()) -> sumo:doc().
sumo_sleep(Player) -> Player.

%% @private
-spec sumo_wakeup(sumo:doc()) -> player().
sumo_wakeup(Doc) -> Doc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a player
-spec new(binary(), binary()) -> player().
new(Name, Password) ->
  Now = ktn_date:now_human_readable(),
  #{ id         => undefined
   , name       => Name
   , password   => Password
   , created_at => Now
   , updated_at => Now
   }.

%% @doc Represents a player as json
-spec to_json(player()) -> map().
to_json(Player) ->
  maps:remove(password, Player).
