%%% @doc /players/[:player_id] handler
-module(lsl_players_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {sr_entities_handler,
         [ init/3
         , rest_init/2
         , allowed_methods/2
         , content_types_accepted/2
         , content_types_provided/2
         , resource_exists/2
         , handle_post/2
         , handle_get/2
         ]}
       ]).

-export([ is_authorized/2
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
    #{ get =>
       #{ tags => ["players"]
        , description => "Returns the list of players"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["players"]
        , description => "Creates a new player"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/players",
  Opts = #{ path => Path
          , model => lsl_players
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  AuthMethod =
    case Method of
      <<"POST">> -> none;
      <<"GET">> -> session
    end,
  lsl_auth:is_authorized([AuthMethod], Req1, State).
