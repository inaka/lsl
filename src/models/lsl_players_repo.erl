%%% @doc Players repository
-module(lsl_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ fetch/2
        , fetch/1
        ]).

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
