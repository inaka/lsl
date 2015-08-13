%%% @doc Players repository
-module(lsl_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/2
        , fetch/2
        , fetch/1
        , all/0
        ]).

%% @doc Creates a new player
-spec register(binary(), binary()) -> lsl_players:player().
register(Name, Password) ->
  case sumo:find_by(lsl_players, [{name, Name}]) of
    [] ->
      PasswordHash = lsl_crypto:hash(Password),
      Player = lsl_players:new(Name, PasswordHash),
      sumo:persist(lsl_players, Player);
    _Players ->
      throw(conflict)
  end.

%% @doc Retrieves a player by it's name a password
-spec fetch(binary(), binary()) -> lsl_players:player() | notfound.
fetch(Name, Password) ->
  case sumo:find_by(lsl_players, [{name, Name}]) of
    [] -> notfound;
    [Player|_] ->
      PasswordHash = lsl_players:password_hash(Player),
      case lsl_crypto:match(Password, PasswordHash) of
        true -> Player;
        false -> notfound
      end
  end.

%% @doc Retrieves a player by it's id
-spec fetch(binary()) -> lsl_players:player() | notfound.
fetch(PlayerId) ->
  sumo:find(lsl_players, PlayerId).

%% @doc Retrieves all players
-spec all() -> [lsl_players:player()].
all() ->
  sumo:find_all(lsl_players).
