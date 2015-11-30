%%% @doc /status handler
-module(lsl_status_handler).
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

-export([handle_get/2]).

-export([ trails/0
        ]).

-type state() :: sr_entities_handler:state().

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["status"]
        , description => "Returns the server status"
        , produces => ["application/json"]
        }
     },
  Path = "/status",
  Opts = #{path => Path, verbose => true},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Reply = sr_json:encode(#{status => <<"ok">>}),
  {Reply, Req, State}.
