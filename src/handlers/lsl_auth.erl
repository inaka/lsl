%%% @doc Default rest handler auth implementation
-module(lsl_auth).
-author('elbrujohalcon@inaka.net').

-export([is_authorized/3]).

-type authorization_mechanism() :: none | player | session.
-export_type([authorization_mechanism/0]).

-type state() :: #{player => undefined | lsl_players:player()}.
-export_type([state/0]).

-spec is_authorized(
  [authorization_mechanism(), ...], cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized([DefaultMech|_] = Mechanisms, Req, State) ->
  is_authorized(Mechanisms, DefaultMech, Req, State).

is_authorized([], DefaultMech, Req, State) ->
  Realm = atom_to_binary(DefaultMech, utf8),
  {{false, <<"Basic realm=\"", Realm/binary, "\"">>}, Req, State};
is_authorized([none|_Rest], _DefaultMech, Req, State) ->
  {true, Req, State};
is_authorized([player|Rest], DefaultMech, Req, State) ->
  case credentials(Req) of
    {undefined, Req1} -> is_authorized(Rest, DefaultMech, Req1, State);
    {{Name, Password}, Req1} ->
      is_authorized(
        lsl_players_repo:fetch(Name, Password), Rest, DefaultMech, Req1, State)
  end;
is_authorized([session|Rest], DefaultMech, Req, State) ->
  case credentials(Req) of
    {undefined, Req1} -> is_authorized(Rest, DefaultMech, Req1, State);
    {{Token, Secret}, Req1} ->
      is_authorized(
        lsl_sessions_repo:fetch_player(Token, Secret),
        Rest, DefaultMech, Req1, State)
  end.

is_authorized(notfound, Rest, DefaultMech, Req, State) ->
  is_authorized(Rest, DefaultMech, Req, State);
is_authorized(Player, _Rest, _DefaultMech, Req, State) ->
  {true, Req, State#{player => Player}}.

credentials(Req) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, Credentials}, Req1} ->
      {Credentials, Req1};
    {ok, _, Req1} ->
      {undefined, Req1}
  end.
