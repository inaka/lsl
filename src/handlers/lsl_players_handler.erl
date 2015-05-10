%%% @doc POST|GET|DELETE /players/[:player_id] handler
-module(lsl_players_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_base_handler,
         [ init/3
         , rest_init/2
         , content_types_accepted/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_post/2
        , handle_get/2
        , is_authorized/2
        ]).

-type state() :: lsl_base_handler:state().

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  AuthMethod =
    case Method of
      <<"POST">> -> none;
      <<"GET">> -> session
    end,
  lsl_base_handler:is_authorized([AuthMethod], Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Name, Password} = parse_body(Body),
    Player = lsl:register_player(Name, Password),
    PlayerId = lsl_players:id(Player),
    RespBody = lsl_json:encode(lsl_players:to_json(Player)),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {{true, <<"/players/", PlayerId/binary>>}, Req2, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  try
    RespBody =
      lsl_json:encode(
        [lsl_players:to_json(Player) || Player <- lsl:fetch_players()]),
    {RespBody, Req, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

parse_body(Body) ->
  Json = lsl_json:decode(Body),
  case {maps:get(<<"name">>, Json, null),
        maps:get(<<"password">>, Json, null)} of
    {null, _} -> throw({missing_field, <<"name">>});
    {_, null} -> throw({missing_field, <<"password">>});
    {Name, Password} -> {Name, Password}
  end.
