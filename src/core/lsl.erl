%%% @doc Main Application Module
-module(lsl).
-author('elbrujohalcon@inaka.net').

-behaviour(application).

-export([ start/0
        , stop/0
        , start/2
        , start_phase/3
        , stop/1
        ]).

-export([ register_player/2
        , fetch_player/2
        , fetch_player/1
        , open_session/1
        , close_session/1
        , can_close_session/2
        , fetch_session_player/2
        , fetch_players/0
        , start_match/3
        , fetch_ai/1
        , find_matches/2
        , fetch_match/1
        , is_match/1
        , play/5
        , is_playing/2
        , is_current_player/2
        , stop_match/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> application:ensure_all_started(?MODULE).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) -> {ok, self()}.

%% @private
-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  application:stop(mnesia),
  mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(lsl, http_port, 8383),
  ListenerCount = application:get_env(lsl, http_listener_count, 10),

  Routes =
    [{'_',
      [ {"/status", lsl_status_handler,  []}
      , {"/players", lsl_players_handler, []}
      , {"/sessions/:session_token", lsl_session_handler, []}
      , {"/sessions", lsl_sessions_handler, []}
      , {"/matches/:match_id", lsl_match_handler, []}
      , {"/matches", lsl_matches_handler, []}
      ]
     }
    ],
  Dispatch = cowboy_router:compile(Routes),
  case cowboy:start_http(
        lsl_http_listener, ListenerCount, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Core functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new player
-spec register_player(binary(), binary()) -> lsl_players:player().
register_player(Name, Password) -> lsl_players_repo:register(Name, Password).

%% @doc Retrieves a player given its name and password
-spec fetch_player(binary(), binary()) -> lsl_players:player() | notfound.
fetch_player(Name, Password) -> lsl_players_repo:fetch(Name, Password).

%% @doc Retrieves a player given its id
-spec fetch_player(binary()) -> lsl_players:player() | notfound.
fetch_player(PlayerId) -> lsl_players_repo:fetch(PlayerId).

%% @doc Generates a new session for the player
-spec open_session(binary()) -> lsl_sessions:session().
open_session(PlayerId) -> lsl_sessions_repo:open(PlayerId).

%% @doc Deletes a session
-spec close_session(binary()) -> boolean().
close_session(SessionToken) -> lsl_sessions_repo:close(SessionToken).

%% @doc Is the player allowed to close the session?
-spec can_close_session(binary(), binary()) -> boolean().
can_close_session(PlayerId, SessionToken) ->
  lsl_sessions_repo:can_close(PlayerId, SessionToken).

%% @doc Retrieves a player given the token and secret for the session
-spec fetch_session_player(binary(), binary()) ->
  lsl_players:player() | notfound.
fetch_session_player(Token, Secret) ->
  case lsl_sessions_repo:fetch(Token, Secret) of
    notfound -> notfound;
    Session -> lsl_players_repo:fetch(lsl_sessions:player_id(Session))
  end.

%% @doc Retrieves all players
-spec fetch_players() -> [lsl_players:player(),...].
fetch_players() -> lsl_players_repo:all().

%% @doc Retrieves a list of matches for a particular player
-spec find_matches(lsl_players:player(), all | lsl_matches:status()) ->
  [lsl_matches:match()].
find_matches(Player, Status) -> lsl_matches_repo:find(Player, Status).

%% @doc Retrieves a match
-spec fetch_match(binary()) -> notfound | lsl_matches:match().
fetch_match(MatchId) -> lsl_matches_repo:fetch(MatchId).

%% @doc Starts a new match
-spec start_match(binary(), module()|binary(), pos_integer()) ->
  lsl_matches:match().
start_match(PlayerId, Rival, Rows) ->
  lsl_matches_repo:start(PlayerId, Rival, Rows).

%% @doc Retrieves an AI module
-spec fetch_ai(binary()) -> module() | notfound.
fetch_ai(AIId) -> lsl_ai:fetch(AIId).

%% @doc Is this a valid match id?
-spec is_match(binary()) -> boolean().
is_match(MatchId) -> lsl_matches_repo:is_match(MatchId).

%% @doc Makes a move
-spec play(
  binary(), binary(), lsl_core:row(), lsl_core:col(), lsl_core:length()) ->
  lsl_matches:match().
play(MatchId, PlayerId, Row, Col, Length) ->
  lsl_matches_repo:play(MatchId, PlayerId, Row, Col, Length).

%% @doc Is the player playing that game
-spec is_playing(binary(), binary()) -> boolean().
is_playing(MatchId, PlayerId) -> lsl_matches_repo:is_playing(MatchId, PlayerId).

%% @doc Is the player the current player for that game
-spec is_current_player(binary(), binary()) -> boolean().
is_current_player(MatchId, PlayerId) ->
  lsl_matches_repo:is_current_player(MatchId, PlayerId).

%% @doc Deletes a match
-spec stop_match(binary()) -> boolean().
stop_match(MatchId) -> lsl_matches_repo:stop(MatchId).
