%%% @doc /sessions/[:session_token] handler
-module(lsl_sessions_handler).
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
        , is_authorized/2
        , forbidden/2
        , delete_resource/2
        ]).

-type state() :: lsl_base_handler:state().

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  case cowboy_req:binding(session_token, Req) of
    {undefined, Req1} ->
      {[<<"POST">>], Req1, State};
    {Token, Req1} ->
      {[<<"DELETE">>], Req1, State#{binding => Token}}
  end.

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State = #{binding := _}) ->
  lsl_base_handler:is_authorized([session, player], Req, State);
is_authorized(Req, State) ->
  lsl_base_handler:is_authorized([player], Req, State).

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
forbidden(Req, State = #{binding := Token}) ->
  try
    #{player := Player} = State,
    PlayerId = lsl_players:id(Player),
    {not lsl:can_close_session(PlayerId, Token), Req, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end;
forbidden(Req, State) ->
  {false, Req, State}.

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    #{player := Player} = State,
    Session = lsl:open_session(lsl_players:id(Player)),
    SessionToken = lsl_sessions:token(Session),
    RespBody = lsl_json:encode(lsl_sessions:to_json(Session)),
    Req1 = cowboy_req:set_resp_body(RespBody, Req),
    {{true, <<"/sessions/", SessionToken/binary>>}, Req1, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
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
