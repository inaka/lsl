%%% @doc /ai-players handler
-module(lsl_ai_players_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {sr_entities_handler,
         [ init/3
         , rest_init/2
         , allowed_methods/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ handle_get/2
        , is_authorized/2
        , trails/0
        ]).

-type state() :: sr_entities_handler:state().

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["players"]
        , description => "Returns the list of AI players"
        , produces => ["application/json"]
        }
     },
  Path = "/ai-players",
  Opts = #{path => Path, verbose => true},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_auth:is_authorized([session], Req, State).

-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  RespBody =
    sr_json:encode(
      [lsl_ai:to_json(Player) || Player <- lsl_ai:all()]),
  {RespBody, Req, State}.
