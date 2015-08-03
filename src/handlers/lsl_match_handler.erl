%%% @doc /matches/:match_id handler
-module(lsl_match_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_base_handler,
         [ init/3
         , rest_init/2
         , is_authorized/2
         , content_types_accepted/2
         , content_types_provided/2
         ]}
       ]).

-export([ allowed_methods/2
        , forbidden/2
        , resource_exists/2
        , handle_get/2
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

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.
resource_exists(Req, State) ->
  #{binding := MatchId} = State,
  {lsl:is_match(MatchId), Req, State}.

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
forbidden(Req, State) ->
  {MatchId, Req1} = cowboy_req:binding(match_id, Req),
  #{player := Player} = State,
  PlayerId = lsl_players:id(Player),
  Forbidden = lsl:is_match(MatchId) and not lsl:is_playing(MatchId, PlayerId),
  {Forbidden, Req1, State#{binding => MatchId}}.

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{binding := MatchId, player := Player} = State,
  try
    Match = lsl:fetch_match(MatchId),
    RespBody =
      lsl_json:encode(lsl_matches:to_json(Match, lsl_players:id(Player))),
    {RespBody, Req, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.
