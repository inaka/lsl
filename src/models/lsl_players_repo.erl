%%% @doc Players repository
-module(lsl_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/2
        , fetch/2
        ]).

%% @doc Creates a new player
-spec register(binary(), binary()) -> lsl_players:player().
register(Name, Password) ->
  case sumo:find_by(lsl_players, [{name, Name}]) of
    [] ->
      PasswordHash = erlpass:hash(Password),
      Player = lsl_players:new(Name, PasswordHash),
      sumo:persist(lsl_players, Player);
    _Players ->
      throw(conflict)
  end.

-spec fetch(binary(), binary()) -> lsl_players:player() | notfound.
fetch(Name, Password) ->
  case sumo:find_by(lsl_players, [{name, Name}]) of
    [] -> notfound;
    [Player|_] ->
      PasswordHash = lsl_players:password_hash(Player),
      case match(Password, PasswordHash) of
        true -> Player;
        false -> notfound
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Sometimes, bcrypt fails to hash, causing erlpass:match to fail,
%%      so we try 5 times in a row.
match(Password, PasswordHash) -> match(5, Password, PasswordHash).

match(1, Password, PasswordHash) ->
  erlpass:match(Password, PasswordHash);
match(N, Password, PasswordHash) ->
  try
    erlpass:match(Password, PasswordHash)
  catch
    _:_ ->
      match(N - 1, Password, PasswordHash)
  end.
