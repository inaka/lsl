%%% @doc Players repository
-module(lsl_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/2
        ]).

%% @doc Creates a new player
-spec register(binary(), binary()) -> lsl_players:player().
register(Name, Password) ->
  Cypher = erlpass:hash(Password),
  Player = lsl_players:new(Name, Cypher),
  sumo:persist(lsl_players, Player).
