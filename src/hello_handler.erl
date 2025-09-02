-module(hello_handler).
-export([init/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_semantic_conventions/include/trace.hrl").

-define(TRACER_ID, ?MODULE).

init(Req, State) ->
  Method = cowboy_req:method(Req),
  Path = cowboy_req:path(Req),
  logger:info("~s ~s", [Method, Path]),
  
  % Handle different endpoints based on the action in State
  case proplists:get_value(action, State) of
    param ->
      handle_param(Req, State);
    exception ->
      handle_exception(Req, State);
    _ ->
      handle_default(Req, State)
  end.

handle_default(Req, State) ->
  ?with_span(spantest, #{attributes => [{?HTTP_SCHEME, <<"https">>}], kind => ?SPAN_KIND_SERVER}, fun(_) ->
  ?set_attributes([{'my-attribute', <<"my-value">>},
                                {another_attribute, <<"value-of-attribute">>}]),
  ?set_status(?OTEL_STATUS_ERROR, <<"this is not ok">>),
  Response = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello from OTP24 Cowboy!">>,
    Req
  ),
  {ok, Response, State}
  end).
handle_param(Req, State) ->
  Id = cowboy_req:binding(id, Req),
  logger:info("Parameter received: ~s", [Id]),
  
  Response = iolist_to_binary(["Parameter logged: ", Id]),
  Req2 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    Response,
    Req
  ),
  {ok, Req2, State}.

  handle_exception(_Req, _State) ->
  try 
    erlang:error(badarg) of
     _ -> ok
  catch
    Class:Reason:Stacktrace ->
      logger:error("Caught ~p:~p ~p", [Class, Reason, Stacktrace]),
      erlang:raise(Class, Reason, Stacktrace)
  end.
