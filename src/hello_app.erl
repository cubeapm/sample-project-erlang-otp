-module(hello_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  % opentelemetry_cowboy:setup(),
  {ok, Pid} = hello_sup:start_link(),
  {ok, Pid}.

stop(_State) ->
  ok.