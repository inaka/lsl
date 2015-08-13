%%% @doc Default rest handler implementation
-module(lsl_base_handler).
-author('elbrujohalcon@inaka.net').

-export([ init/3
        , rest_init/2
        , is_authorized/2
        , is_authorized/3
        , content_types_accepted/2
        , content_types_provided/2
        , resource_exists/2
        ]).

-type state() :: #{ player => undefined | lsl_players:player()
                  , binding => undefined | binary()
                  }.
-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Specs (to be copied to other modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), state()) ->
  {upgrade, protocol, cowboy_rest}.
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), term()}.
-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  Req1 = lsl_web_utils:announce_req(Req, []),
  {ok, Req1, #{}}.

is_authorized(Req, State) ->
  is_authorized([session], Req, State).

content_types_accepted(Req, State) ->
  {Handler, Req1} =
    case cowboy_req:method(Req) of
      {<<"POST">>, Req0} -> {handle_post, Req0};
      {<<"PATCH">>, Req0} -> {handle_patch, Req0}
    end,
  ContentTypes = [{{<<"application">>, <<"json">>, '*'}, Handler}],
  {ContentTypes, Req1, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_authorized(
  [lsl_web_utils:authorization_mechanism(), ...], cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized([DefaultMech|_] = Mechanisms, Req, State) ->
  is_authorized(Mechanisms, DefaultMech, Req, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        lsl:fetch_player(Name, Password), Rest, DefaultMech, Req1, State)
  end;
is_authorized([session|Rest], DefaultMech, Req, State) ->
  case credentials(Req) of
    {undefined, Req1} -> is_authorized(Rest, DefaultMech, Req1, State);
    {{Token, Secret}, Req1} ->
      is_authorized(
        lsl:fetch_session_player(Token, Secret), Rest, DefaultMech, Req1, State)
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
