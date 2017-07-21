%%% @doc /status handler
-module(lsl_status_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_provided/2
        , resource_exists/2
        , handle_get/2
        ]).

-export([trails/0]).

-type state() :: sr_state:state() | #{}.

-type options() :: #{path => string(), verbose => boolean()}.

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

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Teansport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @hidden
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

%% @hidden
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% @hidden
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%% @hidden
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {true, Req, State}.

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Reply = sr_json:encode(#{status => <<"ok">>}),
  {Reply, Req, State}.
