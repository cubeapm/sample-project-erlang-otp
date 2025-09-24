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
    external ->
      handle_external(Req, State);
    _ ->
      handle_default(Req, State)
  end.

handle_default(Req, State) ->
  Method = cowboy_req:method(Req),
  Path = cowboy_req:path(Req),
  % Do not set path in Route. It needs to set to regex like in handler
  Route = <<"/">>,
  SpanName = <<Method/binary, " ", Route/binary>>,
  ?with_span(SpanName, #{attributes => [{?HTTP_SCHEME, <<"http">>}], kind => ?SPAN_KIND_SERVER}, fun(_) ->
    ?set_attributes([{?HTTP_METHOD, Method},{?HTTP_ROUTE, Path}]),
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
  Method = cowboy_req:method(Req),
  Path = cowboy_req:path(Req),
  % Do not set path in Route. It needs to set to regex like in handler
  Route = <<"/param/{paramName}">>,
  SpanName = <<Method/binary, " ", Route/binary>>,
  ?with_span(SpanName, #{attributes => [{?HTTP_SCHEME, <<"https">>}], kind => ?SPAN_KIND_SERVER}, fun(_) ->
    ?set_attributes([{?HTTP_METHOD, Method},{?HTTP_ROUTE, Path}]),
  Response = iolist_to_binary(["Parameter logged: ", Id]),
  Req2 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    Response,
    Req
  ),
  {ok, Req2, State}
  end).

handle_external(Req, State) ->
  Method = cowboy_req:method(Req),
  Path = cowboy_req:path(Req),
  
  Route = <<"/external">>,
  SpanName = <<Method/binary, " ", Route/binary>>,
  
  ?with_span(SpanName, #{attributes => [{?HTTP_SCHEME, <<"http">>}], kind => ?SPAN_KIND_SERVER}, fun(_) ->
    ?set_attributes([{?HTTP_METHOD, Method}, {?HTTP_ROUTE, Path}]),

    % Make external HTTP call with OpenTelemetry instrumentation
    ExternalResponse = make_external_http_call(),
    Response = iolist_to_binary(["External call result: ", ExternalResponse]),
    
    Req2 = cowboy_req:reply(
      200,
      #{<<"content-type">> => <<"text/plain">>},
      Response,
      Req
    ),
    logger:info("DEBUG: Reply sent successfully"),
    {ok, Req2, State}
  end).

handle_exception(_Req, _State) ->
  try 
    erlang:error(badarg)
  catch
    Class:Reason:Stacktrace ->
      logger:error("Caught ~p:~p ~p", [Class, Reason, Stacktrace]),
      erlang:raise(Class, Reason, Stacktrace)
  end.

% Make external HTTP call with OpenTelemetry instrumentation
make_external_http_call() ->
  logger:info("DEBUG: Making external HTTP call to httpbin.org"),
  
  Url = "http://localhost:8080/",
  SpanName = <<"HTTP GET">>,
  
  ?with_span(SpanName, #{kind => ?SPAN_KIND_CLIENT}, fun(_) ->
    ?set_attributes([
      {?HTTP_METHOD, <<"GET">>},
      {?HTTP_URL, list_to_binary(Url)},
      {?HTTP_SCHEME, <<"http">>},
      {?NET_PEER_NAME, <<"localhost">>},
      {?NET_PEER_PORT, 8080}
    ]),
    
    try
      % Using httpc to make a simple GET request
      Options = [],
      Headers = [],
      Request = {Url, Headers},
      
      case httpc:request(get, Request, Options, []) of
        {ok, {{_, 200, _}, _, Body}} ->
          logger:info("DEBUG: External HTTP call successful", [Body]),
          ?set_attributes([
            {?HTTP_STATUS_CODE, 200},
            {?HTTP_ROUTE, <<"/">>}
          ]),
          "HTTP call successful";
        {ok, {{_, StatusCode, _}, _, _}} ->
          logger:warning("DEBUG: External HTTP call returned status: ~p", [StatusCode]),
          ?set_attributes([{?HTTP_STATUS_CODE, StatusCode}]),
          "HTTP call returned non-200 status";
        {error, Reason} ->
          logger:error("DEBUG: External HTTP call failed: ~p", [Reason]),
          ?set_attributes([{?HTTP_STATUS_CODE, 500},{<<"error">>, true}, {<<"error.message">>, list_to_binary(io_lib:format("HTTP request failed: ~p", [Reason]))}]),
          "HTTP call failed"
      end
    catch
      Class:Stacktrace ->
        logger:error("DEBUG: External HTTP call exception: ~p:~p ~p", [Class, Stacktrace]),
        "HTTP call exception"
    end
  end).
