%%% @doc /sessions handler
-module(lsl_sessions_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , content_types_accepted/2
          , content_types_provided/2
          , resource_exists/2
          ]
        }]).

-export([ is_authorized/2
        , handle_post/2
        , trails/0
        ]).

-type state() :: sr_entities_handler:state().

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ post =>
       #{ tags => ["sessions"]
        , description => "Creates a new session"
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/sessions",
  Opts = #{ path => Path
          , model => lsl_sessions
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_auth:is_authorized([player], Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  Player = sr_state:retrieve(player, State, undefined),
  Session = lsl_sessions:new(lsl_players:id(Player)),
  sr_entities_handler:handle_post(Session, Req, State).
