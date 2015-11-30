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

-export([ fetch_player/2
        , fetch_player/1
        , close_session/1
        , can_close_session/2
        , fetch_session_player/2
        , fetch_ai_players/0
        , start_match/3
        , fetch_ai/1
        , find_matches/2
        , is_match/1
        , play/5
        , is_playing/2
        , is_current_player/2
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
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(lsl, http_port, 8383),
  ListenerCount = application:get_env(lsl, http_listener_count, 10),

  Handlers =
    [ lsl_ai_players_handler
    , lsl_matches_handler
    , lsl_players_handler
    , lsl_sessions_handler
    , lsl_single_session_handler
    , lsl_single_match_handler
    , lsl_status_handler
    , cowboy_swagger_handler
    ],
  Routes = trails:trails(Handlers),
  trails:store(Routes),
  Dispatch = trails:single_host_compile(Routes),

  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  case cowboy:start_http(lsl_server, ListenerCount, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Core functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Retrieves a player given its name and password
-spec fetch_player(binary(), binary()) -> lsl_players:player() | notfound.
fetch_player(Name, Password) -> lsl_players_repo:fetch(Name, Password).

%% @doc Retrieves a player given its id
-spec fetch_player(binary()) -> lsl_players:player() | notfound.
fetch_player(PlayerId) -> lsl_players_repo:fetch(PlayerId).

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

%% @doc Retrieves all AI players
-spec fetch_ai_players() -> [module(), ...].
fetch_ai_players() -> lsl_ai:all().

%% @doc Retrieves a list of matches for a particular player
-spec find_matches(lsl_players:player(), all | lsl_matches:status()) ->
  [lsl_matches:match()].
find_matches(Player, Status) -> lsl_matches_repo:find(Player, Status).

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
