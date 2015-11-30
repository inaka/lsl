%%% @doc utilities for web handlers
-module(lsl_web_utils).
-author('elbrujohalcon@inaka.net').

-export([handle_exception/3]).

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
    {halt, cowboy_req:req(), term()}.
handle_exception({missing_field, Field}, Req, State) ->
  Response = sr_json:encode(#{error => <<"missing field: ", Field/binary>>}),
  halt(Response, Req, State);
handle_exception({invalid_field, Field}, Req, State) ->
  Response = sr_json:encode(#{error => <<"invalid field: ", Field/binary>>}),
  halt(Response, Req, State);
handle_exception(badjson, Req, State) ->
  Response = sr_json:encode(#{error => <<"invalid json">>}),
  halt(Response, Req, State);
handle_exception(out_of_bounds, Req, State) ->
  Response = sr_json:encode(#{error => <<"out of bounds">>}),
  halt(Response, Req, State);
handle_exception(Reason, Req, State) ->
  ok = lager:error("~p. Stack Trace: ~p", [Reason, erlang:get_stacktrace()]),
  {ok, Req1} =
    try cowboy_req:reply(500, Req)
    catch
      _:Error ->
        ok = lager:critical(
          "~p trying to report error through cowboy. Stack Trace: ~p",
          [Error, erlang:get_stacktrace()]),
        {ok, Req}
    end,
  {halt, Req1, State}.

halt(Response, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, [], Response, Req),
  {halt, Req1, State}.
