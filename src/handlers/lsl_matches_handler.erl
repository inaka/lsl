%%% @doc /matches handler
-module(lsl_matches_handler).
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
         ]}
       ]).

-export([ is_authorized/2
        , handle_post/2
        , handle_get/2
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
       #{ tags => ["matches"]
        , description => "Returns the list of matches"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["matches"]
        , description => "Creates a new match"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/matches",
  Opts = #{ path => Path
          , model => lsl_matches
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  lsl_base_handler:is_authorized([session], Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    #{player := Player} = State,
    PlayerId = lsl_players:id(Player),
    {ok, Body, Req1} = cowboy_req:body(Req),
    Match =
      case parse_body(Body) of
        {player, Rival, Rows} ->
          lsl:start_match(PlayerId, lsl_players:id(Rival), Rows);
        {ai, AI, Rows} ->
          lsl:start_match(PlayerId, AI, Rows)
      end,
    MatchId = lsl_matches:id(Match),
    RespBody = sr_json:encode(lsl_matches:to_json(Match, PlayerId)),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {{true, <<"/matches/", MatchId/binary>>}, Req2, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

%% @todo remove when https://github.com/inaka/sumo_rest/issues/8 is fixed
-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{player := Player} = State,
  try
    {QsStatus, Req1} = cowboy_req:qs_val(<<"status">>, Req, <<"all">>),
    Status = parse_qs(QsStatus),
    RespBody =
      sr_json:encode(
        [ lsl_matches:to_json(Match, lsl_players:id(Player))
        || Match <- lsl:find_matches(Player, Status)
        ]),
    {RespBody, Req1, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

parse_body(Body) ->
  Json = sr_json:decode(Body),
  DefaultRows = application:get_env(lsl, default_rows, 5),
  case { maps:get(<<"rival">>, Json, null)
       , maps:get(<<"rows">>, Json, DefaultRows)
       } of
    {null, _} -> throw({missing_field, <<"rival">>});
    {_, Rows} when not is_integer(Rows) -> throw({invalid_field, <<"rows">>});
    {_, Rows} when Rows < 2 -> throw({invalid_field, <<"rows">>});
    {RivalId, Rows} -> parse_body(RivalId, Rows)
  end.
parse_body(RivalId, Rows) ->
  case lsl:fetch_ai(RivalId) of
    notfound ->
      case lsl:fetch_player(RivalId) of
        notfound -> throw({invalid_field, <<"rival">>});
        Rival -> {player, Rival, Rows}
      end;
    AI -> {ai, AI, Rows}
  end.

parse_qs(<<"all">>) -> all;
parse_qs(<<"won">>) -> won;
parse_qs(<<"lost">>) -> lost;
parse_qs(<<"playing">>) -> playing;
parse_qs(_) -> throw({invalid_field, <<"status">>}).
