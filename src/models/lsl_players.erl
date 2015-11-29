%%% @doc Players document
-module(lsl_players).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-opaque player() ::
  #{
    name => binary(),
    password_hash => binary(),
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([player/0]).

-export([new/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([to_json/1, from_json/1, update/2, uri_path/1, id/1]).
-export([name/1, password_hash/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(name,          binary,   [id, not_null])
    , sumo:new_field(password_hash, binary,   [not_null])
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ]).

%% @private
-spec sumo_sleep(player()) -> sumo:doc().
sumo_sleep(Player) -> Player.

%% @private
-spec sumo_wakeup(sumo:doc()) -> player().
sumo_wakeup(Doc) -> Doc.

-spec to_json(player()) -> sumo_rest_doc:json().
to_json(Player) ->
  maps:remove(password_hash, Player).

-spec from_json(sumo_rest_doc:json()) ->
  {ok, player()} | {error, sumo_rest_doc:reason()}.
from_json(Json) ->
  try
    Name = maps:get(<<"name">>, Json),
    PasswordHash = lsl_crypto:hash(maps:get(<<"password">>, Json)),
    {ok, new(Name, PasswordHash)}
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(player(), sumo_rest_doc:json()) ->
  {ok, player()} | {error, sumo_rest_doc:reason()}.
update(Player, Json) ->
  case from_json(Json) of
    {error, Reason} -> {error, Reason};
    {ok, Updates} ->
      UpdatedPlayer = maps:merge(Player, Updates),
      {ok, UpdatedPlayer#{updated_at => ktn_date:now_human_readable()}}
  end.

-spec uri_path(player()) -> iodata().
uri_path(Player) -> name(Player).

-spec id(player()) -> term().
id(Player) -> name(Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a player
-spec new(binary(), binary()) -> player().
new(Name, PasswordHash) ->
  Now = ktn_date:now_human_readable(),
  #{ id             => undefined
   , name           => Name
   , password_hash  => PasswordHash
   , created_at     => Now
   , updated_at     => Now
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc name
-spec name(player()) -> binary().
name(#{name := Name}) -> Name.

%% @doc password_hash
-spec password_hash(player()) -> binary().
password_hash(#{password_hash := PasswordHash}) -> PasswordHash.
