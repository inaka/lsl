%%% @doc /matches/[:match_id] handler
-module(lsl_matches_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
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

-type state() :: lsl_base_handler:state().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mixin Specs
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


-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    #{player := Player} = State,
    PlayerId = lsl_players:id(Player),
    {ok, Body, Req1} = cowboy_req:body(Req),
    Match =
      case parse_body(Body) of
        {player, Rival, Rows} ->
          lsl:start_match(PlayerId, lsl_players:id(Rival), Rows);
        {ai, AI, Rows} ->
          lsl:start_match(PlayerId, AI, Rows)
      end,
    MatchId = lsl_matches:id(Match),
    RespBody = lsl_json:encode(lsl_matches:to_json(Match, PlayerId)),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {{true, <<"/matches/", MatchId/binary>>}, Req2, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

parse_body(Body) ->
  Json = lsl_json:decode(Body),
  DefaultRows = application:get_env(lsl, default_rows, 5),
  case { maps:get(<<"rival">>, Json, null)
       , maps:get(<<"rows">>, Json, DefaultRows)
       } of
    {null, _} -> throw({missing_field, <<"rival">>});
    {_, Rows} when not is_integer(Rows) -> throw({invalid_field, <<"rows">>});
    {_, Rows} when Rows < 2 -> throw({invalid_field, <<"rows">>});
    {RivalId, Rows} ->
      case lsl:fetch_ai(RivalId) of
        notfound ->
          case lsl:fetch_player(RivalId) of
            notfound -> throw({invalid_field, <<"rival">>});
            Rival -> {player, Rival, Rows}
          end;
        AI -> {ai, AI, Rows}
      end
  end.
