%%% @doc Sessions document
-module(lsl_sessions).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-opaque session() ::
  #{ token => binary()
   , secret => undefined | binary()
   , secret_hash => binary()
   , player_id => binary()
   , created_at => dcn_datetime:datetime()
   }.
-export_type([session/0]).

-export([new/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([to_json/1, from_json/1, update/2, uri_path/1]).
-export([token/1, secret_hash/1, secret/1, secret/2, player_id/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(token,       binary,   [id, not_null])
    , sumo:new_field(secret_hash, binary,   [not_null])
    , sumo:new_field(player_id,   binary,   [not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    ]).

%% @private
-spec sumo_sleep(session()) -> sumo:doc().
sumo_sleep(Session) -> maps:remove(secret, Session).

%% @private
-spec sumo_wakeup(sumo:doc()) -> session().
sumo_wakeup(Doc) -> Doc#{secret => undefined}.

%% @private
-spec to_json(session()) -> map().
to_json(Session) ->
  maps:remove(secret_hash, Session).

-spec from_json(sumo_rest_doc:json()) -> no_return().
from_json(_Json) -> throw(no_simple_parsing).

-spec update(session(), sumo_rest_doc:json()) -> no_return().
update(_Session, _Changes) -> throw(no_simple_parsing).

-spec uri_path(session()) -> iodata().
uri_path(Session) -> token(Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a session
-spec new(binary(), binary()) -> session().
new(PlayerId, SecretHash) ->
  Now = ktn_date:now_human_readable(),
  #{ token        => undefined
   , secret_hash  => SecretHash
   , player_id    => PlayerId
   , created_at   => Now
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc token
-spec token(session()) -> binary().
token(#{token := Token}) -> Token.

%% @doc secret_hash
-spec secret_hash(session()) -> binary().
secret_hash(#{secret_hash := SecretHash}) -> SecretHash.

%% @doc secret
-spec secret(session()) -> binary().
secret(#{secret := Secret}) -> Secret.

%% @doc secret setter
-spec secret(session(), binary()) -> session().
secret(Session, Secret) -> Session#{secret := Secret}.

%% @doc player id
-spec player_id(session()) -> binary().
player_id(#{player_id := PlayerId}) -> PlayerId.
