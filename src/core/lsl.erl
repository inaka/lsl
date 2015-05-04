-module(lsl).
-author('elbrujohalcon@inaka.net').

-behaviour(application).

-export([ start/0
        , stop/0
        , start/2
        , start_phase/3
        , stop/1
        ]).

-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> application:ensure_all_started(?MODULE).

-spec stop() -> ok | {error, term()}.
stop() -> application:stop(?MODULE).

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) -> {ok, self()}.

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(lsl, http_port, 8383),
  ListenerCount = application:get_env(lsl, http_listener_count, 10),

  Routes =
    [{'_',
      [ {"/status", lsl_status_handler, []}
      ]
     }
    ],
  Dispatch = cowboy_router:compile(Routes),
  case cowboy:start_http(
        lsl_http_listener, ListenerCount, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

-spec stop([]) -> ok.
stop([]) -> ok.
