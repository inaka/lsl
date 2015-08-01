%%% @doc /sessions handler
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
allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_base_handler:is_authorized([player], Req, State).

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
