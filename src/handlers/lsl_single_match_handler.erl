%%% @doc /matches/:match_id handler
-module(lsl_single_match_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , delete_resource/2
          ]
        }]).

-export([ forbidden/2
        , handle_patch/2
        , trails/0
        ]).

-type state() :: sr_single_entity_handler:state().

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Id =
    #{ name => id
     , in => path
     , description => <<"Match Id">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["matches"]
        , description => "Returns an match"
        , produces => ["application/json"]
        , parameters => [Id]
        }
     , patch =>
       #{ tags => ["matches"]
        , description => "Registers your move"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , delete =>
       #{ tags => ["matches"]
        , description => "Deletes an match"
        , parameters => [Id]
        }
     },
  Path = "/matches/:id",
  Opts = #{ path => Path
          , model => sr_matches
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
forbidden(Req, State) ->
  {MatchId, Req1} = cowboy_req:binding(match_id, Req),
  #{player := Player} = State,
  PlayerId = lsl_players:id(Player),
  {Forbidden, Req2} =
    case cowboy_req:method(Req1) of
      {<<"PATCH">>, NewReq} ->
        { lsl:is_match(MatchId) and not lsl:is_current_player(MatchId, PlayerId)
        , NewReq
        };
      {_GetOrDelete, NewReq} ->
        { lsl:is_match(MatchId) and not lsl:is_playing(MatchId, PlayerId)
        , NewReq
        }
    end,
  {Forbidden, Req2, State#{binding => MatchId}}.

-spec handle_patch(cowboy_req:req(), state()) ->
    {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_patch(Req, State) ->
  try
    #{player := Player, binding := MatchId} = State,
    PlayerId = lsl_players:id(Player),
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Row, Col, Length} = parse_body(Body),
    Match = lsl:play(MatchId, PlayerId, Row, Col, Length),
    RespBody = sr_json:encode(lsl_matches:to_json(Match, PlayerId)),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {true, Req2, State}
  catch
    _:Exception ->
      lsl_web_utils:handle_exception(Exception, Req, State)
  end.

parse_body(Body) ->
  Json = sr_json:decode(Body),
  case { maps:get(<<"row">>,    Json, null)
       , maps:get(<<"col">>,    Json, null)
       , maps:get(<<"length">>, Json, null)
       } of
    {null, _, _} -> throw({missing_field, <<"row">>});
    {_, null, _} -> throw({missing_field, <<"col">>});
    {_, _, null} -> throw({missing_field, <<"length">>});
    {Row, _, _} when not is_integer(Row) -> throw({invalid_field, <<"row">>});
    {_, Col, _} when not is_integer(Col) -> throw({invalid_field, <<"col">>});
    {_, _, Len} when not is_integer(Len) ->throw({invalid_field, <<"length">>});
    {Row, Col, Len} -> {Row, Col, Len}
  end.
