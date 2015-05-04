%%% @doc GET /status handler
-module(lsl_status_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {lsl_base_handler,
         [ init/3
         , rest_init/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , is_authorized/2
        , handle_get/2
        ]).

-type state() :: #{}.

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  {true, Req, State}.

-spec handle_get(cowboy_req:req(), state()) ->
  {halt | binary(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Reply = lsl_json:encode(#{status => <<"ok">>}),
  {Reply, Req, State}.
