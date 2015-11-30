%%% @doc /sessions/:session_token handler
-module(lsl_single_session_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , delete_resource/2
          ]
        }]).

-export([ is_authorized/2
        , forbidden/2
        ]).

-export([ trails/0
        ]).

-type state() :: sr_single_entity_handler:state().

-spec trails() -> trails:trails().
trails() ->
  Id =
    #{ name => id
     , in => path
     , description => <<"Session Token">>
     , required => true
     , type => string
     },
  Metadata =
    #{ delete =>
       #{ tags => ["sessions"]
        , description => "Closes a session"
        , parameters => [Id]
        }
     },
  Path = "/sessions/:id",
  Opts = #{ path => Path
          , model => lsl_sessions
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_auth:is_authorized([session, player], Req, State).

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
forbidden(Req, State) ->
  #{player := Player, id := Token} = State,
  try
    PlayerId = lsl_players:id(Player),
    {not lsl:can_close_session(PlayerId, Token), Req, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.
