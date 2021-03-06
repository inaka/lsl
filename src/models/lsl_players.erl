%%% @doc Players document
-module(lsl_players).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-opaque player() ::
  #{ id => binary()
   , name => binary()
   , password_hash => binary()
   , created_at => binary()
   , updated_at => binary()
   }.
-export_type([player/0]).

-export([new/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([to_json/1, from_json/1, update/2, location/2, uri_path/1, id/1]).
-export([name/1, password_hash/1, duplication_conditions/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            binary, [id, not_null])
    , sumo:new_field(name,          binary, [not_null])
    , sumo:new_field(password_hash, binary, [not_null])
    , sumo:new_field(created_at,    binary, [not_null])
    , sumo:new_field(updated_at,    binary, [not_null])
    ]).

%% @private
-spec sumo_sleep(player()) -> sumo:model().
sumo_sleep(Player) -> Player.

%% @private
-spec sumo_wakeup(sumo:model()) -> player().
sumo_wakeup(Doc) -> Doc.

-spec to_json(player()) -> #{atom() => binary()}.
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
update(Player, _Json) -> {ok, Player}.

-spec location(player(), sumo_rest_doc:json()) -> binary().
location(#{id := Id}, Path) -> iolist_to_binary([Path, "/", Id]).

-spec uri_path(player()) -> iodata().
uri_path(#{id := Id}) -> <<$/, Id/binary>>.

-spec id(player()) -> binary().
id(#{id := Id}) -> Id.

-spec duplication_conditions(player()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(#{name := Name}) -> {name, Name}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a player
-spec new(binary(), binary()) -> player().
new(Name, Password) ->
  PasswordHash = lsl_crypto:hash(Password),
  Now = ktn_date:now_human_readable(),
  #{ id             => Name
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
