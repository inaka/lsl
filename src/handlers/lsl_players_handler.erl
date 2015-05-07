%%% @doc POST|GET|DELETE /players/[:player_id] handler
-module(lsl_players_handler).
-author('elbrujohalcon@inaka.net').

-mixin([
        {lsl_base_handler,
         [ init/3
         , rest_init/2
         , is_authorized/2
         , content_types_accepted/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_post/2
        ]).

-type state() :: #{}.

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | boolean(), cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Name, Password} = parse_body(Body),
    Player = #{id := PlayerId} = lsl:register_player(Name, Password),
    RespBody = lsl_players:to_json(Player),
    {{true, <<"/players/", PlayerId/binary>>}, Req1, State}
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
