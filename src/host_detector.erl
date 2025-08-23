-module(host_detector).
-export([get_resource/1]).

get_resource(_Opts) ->
  Hostname = list_to_binary(net_adm:localhost()),
  Pid = list_to_binary(os:getpid()),
  otel_resource:create(
    [
      {<<"host.name">>, Hostname},
      {<<"process.pid">>, Pid}
    ],
    []
  ).