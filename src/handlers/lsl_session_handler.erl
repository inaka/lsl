%%% @doc /sessions/:session_token handler
-module(lsl_session_handler).
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
        , is_authorized/2
        , forbidden/2
        , delete_resource/2
        ]).

-type state() :: lsl_base_handler:state().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mixin Specs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), state()) ->
  {upgrade, protocol, cowboy_rest}.
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), term()}.
-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) -> {[<<"DELETE">>], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_base_handler:is_authorized([session, player], Req, State).

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
forbidden(Req, State) ->
  {Token, Req1} = cowboy_req:binding(session_token, Req),
  try
    #{player := Player} = State,
    PlayerId = lsl_players:id(Player),
    {not lsl:can_close_session(PlayerId, Token), Req1, State#{binding => Token}}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req1, State#{binding => Token})
  end.

-spec delete_resource(cowboy_req:req(), state()) ->
  {true | halt, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  try
    #{binding := Token} = State,
    lsl:close_session(Token),
    {true, Req, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.